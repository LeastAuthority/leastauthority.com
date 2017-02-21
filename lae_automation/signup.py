#!/usr/bin/python

import time
from base64 import b32encode
from pprint import pformat
import json
from hashlib import sha256

import attr

from eliot import start_action
from eliot.twisted import DeferredContext

from twisted.internet import reactor
from twisted.web.client import Agent
from twisted.python.filepath import FilePath
from twisted.internet.defer import succeed

from lae_automation.initialize import create_stripe_user_bucket
from lae_automation.confirmation import send_signup_confirmation, send_notify_failure

from lae_automation.config import Config
from lae_automation.model import DeploymentConfiguration, SubscriptionDetails
from lae_util.streams import LoggingStream
from lae_util.servers import append_record

from .subscription_manager import network_client

from lae_util.timestamp import format_iso_time

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'


def lookup_product(config, plan_ID):
    ps = [p for p in config.products if p['plan_ID'] == plan_ID]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (plan_ID, len(ps)))

    return ps[0]


def encode_id(ident):
    return b32encode(ident).rstrip("=").lower()


def get_bucket_name(subscription_id, customer_id):
    return "lae-%s-%s" % (encode_id(subscription_id), encode_id(customer_id))


# TODO Replace activate with this
def activate_ex(
        just_activate_subscription,
        send_signup_confirmation,
        send_notify_failure,
        domain,
        secrets_dir,
        automation_config_path,
        server_info_path,
        stdin,
        flapp_stdout_path,
        flapp_stderr_path,
):
    append_record(flapp_stdout_path, "Automation script started.")
    parameters_json = stdin.read()
    (customer_email,
     customer_pgpinfo,
     customer_id,
     plan_id,
     subscription_id) = json.loads(parameters_json)

    (abslogdir_fp,
    stripesecrets_log_fp,
    SSEC2secrets_log_fp,
    signup_log_fp) = create_log_filepaths(
        secrets_dir, plan_id, customer_id, subscription_id,
    )

    append_record(flapp_stdout_path, "Writing logs to %r." % (abslogdir_fp.path,))

    stripesecrets_log_fp.setContent(parameters_json)

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')

    def errhandler(err):
        with flapp_stderr_path.open("a") as fh:
            err.printTraceback(fh)
        return err

    with flapp_stderr_path.open("a") as stderr:
        print >>stderr, "plan_id is %s" % plan_id

    config = Config(automation_config_path.path)
    product = lookup_product(config, plan_id)
    fullname = product['plan_name']

    with flapp_stdout_path.open("a") as stdout:
        print >>stdout, "Signing up customer for %s..." % (fullname,)

    deploy_config = DeploymentConfiguration(
        domain=domain,
        products=config.products,
        s3_access_key_id=config.other["s3_access_key_id"].decode("ascii"),
        s3_secret_key=FilePath(config.other["s3_secret_path"]).getContent().strip().decode("ascii"),

        # Confusingly, this isn't used in the signup codepath. :/
        # Instead, the subscription converger has the value passed to it.
        # This is mostly because updating the product configuration is a pain in the ass.
        # Fix that and then fix this.
        introducer_image=u"",
        storageserver_image=u"",

        log_gatherer_furl=config.other.get("log_gatherer_furl") or None,
        stats_gatherer_furl=config.other.get("stats_gatherer_furl") or None,

        usertoken=None,
        producttoken=None,

        secretsfile=SSEC2_secretsfile,
        serverinfopath=server_info_path.path,

        ssec2_access_key_id=config.other.get("ssec2_access_key_id", None),
        ssec2_secret_path=config.other.get("ssec2_secret_path", None),

        ssec2admin_keypair_name=config.other.get("ssec2admin_keypair_name", None),
        ssec2admin_privkey_path=config.other.get("ssec2admin_privkey_path", None),

        monitor_pubkey_path=config.other.get("monitor_pubkey_path", None),
        monitor_privkey_path=config.other.get("monitor_privkey_path", None),
    )
    subscription = SubscriptionDetails(
        bucketname=get_bucket_name(subscription_id, customer_id),
        oldsecrets=None,
        customer_email=customer_email,
        customer_pgpinfo=customer_pgpinfo,
        product_id=plan_id,
        customer_id=customer_id,
        subscription_id=subscription_id,

        introducer_port_number=None,
        storage_port_number=None,
    )
    a = start_action(
        action_type=u"signup:activate",
        subscription=attr.asdict(subscription),
    )
    with a.context():
        d = DeferredContext(just_activate_subscription(
            deploy_config, subscription, signup_stdout, signup_stderr,
            signup_log_fp.path, None, None,
        ))
        def activate_success(details):
            a = start_action(
                action_type=u"signup:send-confirmation",
                subscription=attr.asdict(details),
            )
            with a.context():
                d = DeferredContext(send_signup_confirmation(
                    details.customer_email, details.external_introducer_furl,
                    None, signup_stdout, signup_stderr,
                ))
                return d.addActionFinish()
        d.addCallback(activate_success)

        def activate_failure(reason):
            # XXX Eliot log reason here too
            a = start_action(action_type=u"signup:send-failure")
            with a.context():
                d = DeferredContext(send_notify_failure(
                    reason, subscription.customer_email, signup_log_fp.path,
                    signup_stdout, signup_stderr,
                ))
                return d.addActionFinish()
        d.addErrback(activate_failure)

        d.addErrback(errhandler)
        d.addBoth(lambda ign: signup_logfile.close())
        return d.addActionFinish()



def activate(domain, secrets_dir, automation_config_path, server_info_path, stdin, flapp_stdout_path, flapp_stderr_path):
    return activate_ex(
        just_activate_subscription,
        send_signup_confirmation,
        send_notify_failure,
        domain,
        secrets_dir,
        automation_config_path,
        server_info_path,
        stdin,
        flapp_stdout_path,
        flapp_stderr_path,
    )


def just_activate_subscription(deploy_config, subscription, stdout, stderr, logfile, clock, smclient):
    print >>stderr, "entering just_activate_subscription call."
    if clock is None:
        clock = reactor

    location = None  # default S3 location for now

    a = start_action(
        action_type=u"signup:bucket-creation",
        key_id=deploy_config.s3_access_key_id,
        secret_key_hash=sha256(deploy_config.s3_secret_key).hexdigest().decode("ascii"),
    )
    with a.context():
        d = create_stripe_user_bucket(
            deploy_config.s3_access_key_id,
            deploy_config.s3_secret_key,
            subscription.bucketname,
            stdout,
            stderr,
            location,
        )
        d = DeferredContext(d)

        print >>stdout, "After create_stripe_account_user_bucket:"
        print >>stdout, pformat(attr.asdict(deploy_config))
        print >>stdout, pformat(attr.asdict(subscription))

        if smclient is None:
            endpoint = deploy_config.subscription_manager_endpoint.asText().encode("utf-8")
            agent = Agent(reactor)
            smclient = network_client(endpoint, agent)

        d.addCallback(
            lambda ignored: provision_subscription(
                clock, deploy_config, subscription, smclient,
            )
        )
        return d.addActionFinish()


def provision_subscription(reactor, deploy_config, details, smclient):
    """
    Create the subscription state in the SubscriptionManager service.

    :param DeploymentConfiguration deploy_config:
    :param SubscriptionDetails details:
    """
    d = smclient.create(details.subscription_id, details)
    def created(details):
        d = _wait_for_service(details.subscription_id)
        d.addCallback(lambda ignored: details)
        return d
    d.addCallback(created)
    return d



def _wait_for_service(subscription_id):
    # XXX Poll Kubernetes or DNS or something looking for matching resources.
    # XXX With a timeout and some error logging.
    return succeed(None)



def create_log_filepaths(parent_dir, stripe_plan_id, stripe_customer_id, stripe_subscription_id):
    timestamp = format_iso_time(time.time())
    fpcleantimestamp = timestamp.replace(':', '')
    logdirname = "%s-%s" % (fpcleantimestamp, get_bucket_name(stripe_customer_id, stripe_subscription_id)[len('lae-') :])
    abslogdir_fp = parent_dir.child(stripe_plan_id).child(logdirname)
    abslogdir_fp.makedirs()
    stripelog_fp = abslogdir_fp.child('stripe')
    SSEC2log_fp = abslogdir_fp.child('SSEC2')
    signuplog_fp = abslogdir_fp.child('signup_logs')
    return abslogdir_fp, stripelog_fp, SSEC2log_fp, signuplog_fp
