#!/usr/bin/env python

#
# Perform an S4 signup and minimally verify the resulting Tahoe-LAFS service.
# See tahoe-test.sh for details of the service verification.
#
# You must have access to a subscription manager to use this tool.  If you
# want to add subscriptions to a deployment on Kubernetes, you can use
# `kubectl port-forward` for this:
#
#     $ kubectl port-forward <pod name> 8000:8000
#
# Usage:
#
#     s4-test.py <subscription manager url> <subscription owner email address>
#
# A rudamentary stress test can be cobbled together with a shell loop:
#
#     for i in $(seq 1 20); do
#         s4-test.py <url> "alice+${i}@example.com"
#     done
#

from __future__ import print_function

from sys import stdout
from time import sleep
from hashlib import sha256
from subprocess import check_call
from tempfile import mkdtemp
from datetime import timedelta

from twisted.internet.task import react
from twisted.web.client import Agent
from twisted.python.filepath import FilePath

from lae_automation.model import SubscriptionDetails
from lae_automation.subscription_manager import network_client
from lae_automation.signup import get_bucket_name

from sys import argv

HERE = FilePath(__file__).parent()

def subscribe(reactor, root_url, email):
    """
    Add a new subscription to the subscription manager.

    :param reactor: The reactor to use for I/O to the manager.

    :param bytes root_url: The root URL of the subscription manager API
        server.

    :param bytes email: An email address to associate with the subscription.

    :return Deferred: A deferred which fires after the subscription has been
        created.
    """
    identifier = sha256(bytes(reactor.seconds())).hexdigest()[:14]
    sid = u"sid_" + identifier
    cid = u"cus_" + identifier

    agent = Agent(reactor)
    client = network_client(root_url, agent)
    subscription = SubscriptionDetails(
        bucketname=get_bucket_name(sid, cid),

        oldsecrets={},
        product_id=u"filler",

        customer_email=email,
        customer_pgpinfo=None,

        customer_id=cid,
        subscription_id=sid,

        introducer_port_number=0,
        storage_port_number=0,
    )
    return client.create(sid, subscription)


def check_subscription(details):
    """
    Given a subscription, try to use the Tahoe-LAFS service that goes with it.

    This blocks until the check is complete.

    :param SubscriptionDetails details: Information about the subscription to
    check, including the introducer furl.
    """
    furl = details.external_introducer_furl
    # If we try to use the furl before it is set up then we put a negative
    # cache item into the DNS server and we have to wait even longer.  Try to
    # wait for DNS to be set up before issuing the first query.
    stdout.write(u"Waiting... ")
    noisy_wait(timedelta(seconds=30))
    print(u"Testing {}".format(furl))

    # If we made this a little more stable then we could avoid the constant
    # pip install'ing of tahoe-lafs.
    tahoe_env = mkdtemp(prefix=u"tahoe-lafs-env")
    check_call([HERE.child("tahoe-test.sh").path, tahoe_env, furl])


def noisy_wait(duration):
    """
    Block for a given amount of time.

    :param timedelta duration: The amount of time for which to block.
    """
    seconds = int(duration.total_seconds())
    for i in range(seconds):
        stdout.write(u"[" + u"#" * i + u"-" * (seconds - i) + u"]")
        stdout.flush()
        sleep(1)
        stdout.write(u"\b" * (seconds + 2))
        stdout.flush()
    print(u"")


@react
def main(reactor):
    """
    Create and verify one S4 subscription.
    """
    root_url = argv[1]
    email = argv[2]

    d = subscribe(reactor, root_url, email)
    d.addCallback(check_subscription)
    return d
