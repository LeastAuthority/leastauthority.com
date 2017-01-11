#!/usr/bin/python

import time
from base64 import b32encode

from twisted.internet import defer, reactor, task
from twisted.python.filepath import FilePath
from twisted.web.client import Agent

from lae_automation.config import Config
from lae_automation.initialize import create_stripe_user_bucket, deploy_EC2_instance, verify_and_store_serverssh_pubkey
from lae_automation.aws.queryapi import TimeoutError, wait_for_EC2_addresses
from lae_automation.server import install_server, bounce_server, NotListeningError
from lae_automation.confirmation import send_signup_confirmation, send_notify_failure
from lae_automation.containers import provision_subscription

from .model import DeploymentConfiguration, SubscriptionDetails
from .subscription_manager import network_client

from lae_util.servers import append_record
from lae_util.timestamp import format_iso_time

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

POLL_TIME = 30

# credit card verification might take 15 minutes, so wait 20.
CC_VERIFICATION_TIME = 20 * 60

# wait 75 seconds before the first poll, then up to 5 minutes for the addresses.
ADDRESS_DELAY_TIME = 75
ADDRESS_WAIT_TIME = 5 * 60

LISTEN_RETRIES = 5
LISTEN_POLL_TIME = 15
VERIFY_POLL_TIME = 5
VERIFY_TOTAL_WAIT = 600


def lookup_product(config, plan_ID):
    ps = [p for p in config.products if p['plan_ID'] == plan_ID]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (plan_ID, len(ps)))

    return ps[0]


def encode_id(ident):
    return b32encode(ident).rstrip("=").lower()

def get_bucket_name(subscription_id, customer_id):
    return "lae-%s-%s" % (encode_id(subscription_id), encode_id(customer_id))


def activate_subscribed_service(deploy_config, subscription, stdout, stderr, logfile, clock=None, smclient=None):
    print >>stderr, "entering activate_subscribed_service call."
    if clock is None:
        clock = reactor

    location = None  # default S3 location for now

    d = create_stripe_user_bucket(
        deploy_config.s3_access_key_id,
        deploy_config.s3_secret_key,
        subscription.bucketname,
        stdout,
        stderr,
        location,
    )

    print >>stdout, "After create_stripe_account_user_bucket %s..." % (deploy_config,)

    if smclient is None:
        endpoint = u"http://{}/".format(
            deploy_config.subscription_manager_hostname
        ).encode("utf-8")
        agent = Agent(reactor)
        smclient = network_client(endpoint, agent)

    d.addCallback(
        lambda ignored: provision_subscription(
            clock, deploy_config, subscription, smclient,
        )
    )
    d.addErrback(lambda f: send_notify_failure(
        f, subscription.customer_email, logfile, stdout, stderr,
    ))
    return d



def replace_server(oldsecrets, amiimageid, instancesize, customer_email, stdout, stderr,
                   secretsfile, logfilename,
                   configpath=None,
                   serverinfopath=None, clock=None):
    config = Config(configpath)
    s3_access_key_id = oldsecrets['access_key_id']
    s3_secretkey   = oldsecrets['secret_key']
    usertoken     = oldsecrets.get('user_token', None)     # DevPay only
    producttoken  = oldsecrets.get('product_token', None)  # DevPay only
    bucketname    = oldsecrets["bucket_name"]

    deploy_config = DeploymentConfiguration(
        products=config.products,
        s3_access_key_id=s3_access_key_id,
        s3_secret_key=s3_secretkey,
        amiimageid=amiimageid,
        instancesize=instancesize,

        usertoken=usertoken,
        producttoken=producttoken,

        secretsfile=secretsfile,
        serverinfopath=serverinfopath,

        ssec2_access_key_id=config["ssec2_access_key_id"],
        ssec2_secret_path=config["ssec2_secret_path"],

        ssec2admin_keypair_name=config["ssec2admin_keypair_name"],
        ssec2admin_privkey_path=config["ssec2admin_privkey_path"],

        monitor_pubkey_path=config["monitor_pubkey_path"],
        monitor_privkey_path=config["monitor_privkey_path"],
    )
    subscription = SubscriptionDetails(
        bucketname=bucketname,
        oldsecrets=oldsecrets,
        customer_email=customer_email,
        customer_pgpinfo=None,

        product_id=None,
        customer_id=None,
        subscription_id=None,
    )
        
    d = deploy_server(deploy_config, subscription, stdout, stderr, clock)
    d.addErrback(lambda f: send_notify_failure(f, customer_email, logfilename, stdout,
                                               stderr))
    return d


def deploy_server(deploy_config, subscription, stdout, stderr, clock=None):
    if clock is None:
        clock = reactor

    ec2accesskeyid = deploy_config.ssec2_access_key_id
    ec2secretpath = deploy_config.ssec2_secret_path
    ec2secretkey = FilePath(ec2secretpath).getContent().strip()

    instancename = subscription.customer_email  # need not be unique

    admin_keypair_name = str(deploy_config.ssec2admin_keypair_name)
    admin_privkey_path = str(deploy_config.ssec2admin_privkey_path)
    monitor_pubkey = FilePath(str(deploy_config.monitor_pubkey_path)).getContent().strip()
    monitor_privkey_path = str(deploy_config.monitor_privkey_path)

    # XXX Here's where we decide whether the new signup goes to a new EC2.
    d = deploy_EC2_instance(
        ec2accesskeyid,
        ec2secretkey,
        EC2_ENDPOINT,
        deploy_config.amiimageid,
        deploy_config.instancesize,
        subscription.bucketname,
        admin_keypair_name,
        instancename,
        stdout,
        stderr,
    )

    def _deployed(instance):
        print >>stdout, "Waiting %d seconds for the server to be ready..." % (ADDRESS_DELAY_TIME,)
        d2 = task.deferLater(clock, ADDRESS_DELAY_TIME, wait_for_EC2_addresses,
                             ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, stdout, stderr, POLL_TIME,
                             ADDRESS_WAIT_TIME, instance.instance_id)

        def _got_addresses(addresses):
            assert len(addresses) == 1, addresses
            (publichost, privatehost) = addresses[0]
            print >>stdout, "The server's public address is %r." % (publichost,)

            d3 = verify_and_store_serverssh_pubkey(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT,
                                                   publichost, VERIFY_POLL_TIME, VERIFY_TOTAL_WAIT,
                                                   stdout, stderr, instance.instance_id)

            def _got_sshfp(ignored):
                retries = LISTEN_RETRIES
                while True:
                    try:
                        install_server(publichost, admin_privkey_path, monitor_pubkey,
                                       monitor_privkey_path, stdout, stderr)
                        break
                    except NotListeningError:
                        retries -= 1
                        if retries <= 0:
                            print >>stdout, "Timed out waiting for EC2 instance to listen for ssh connections."
                            raise TimeoutError()
                        print >>stdout, "Waiting another %d seconds..." % (LISTEN_POLL_TIME)
                        time.sleep(LISTEN_POLL_TIME)
                        continue

                furl = bounce_server(
                    deploy_config,
                    subscription,
                    publichost,
                    admin_privkey_path,
                    privatehost,
                    stdout,
                    stderr,
                )

                # XXX We probably need to rethink this:
                append_record(FilePath(deploy_config.serverinfopath), instance.launch_time, instance.instance_id,
                              publichost)

                print >>stderr, "Signup done."
                d4 = defer.succeed(None)
                if not subscription.oldsecrets:
                    d4.addCallback(
                        lambda ign: send_signup_confirmation(
                            publichost,
                            subscription.customer_email,
                            furl,
                            subscription.customer_pgpinfo,
                            stdout,
                            stderr,
                        )
                    )
                return d4
            d3.addCallback(_got_sshfp)
            return d3
        d2.addCallback(_got_addresses)
        return d2
    d.addCallback(_deployed)
    return d


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
