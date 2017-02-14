#!/usr/bin/python

import time
from base64 import b32encode
from pprint import pformat

import attr

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
    d = just_activate_subscription(
        deploy_config, subscription, stdout, stderr, logfile, clock, smclient,
    )
    d.addErrback(lambda f: send_notify_failure(
        f, subscription.customer_email, logfile, stdout, stderr,
    ))
    return d


def just_activate_subscription(deploy_config, subscription, stdout, stderr, logfile, clock, smclient):
    print >>stderr, "entering just_activate_subscription call."
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

    print >>stdout, "After create_stripe_account_user_bucket:"
    print >>stdout, pformat(attr.asdict(deploy_config))
    print >>stdout, pformat(attr.asdict(subscription))

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
