
from __future__ import unicode_literals, print_function

from functools import partial
from sys import argv
from os import environ

import stripe

from twisted.internet.error import ConnectionRefusedError
from twisted.internet.protocol import ProcessProtocol
from twisted.internet.defer import (
    inlineCallbacks,
    returnValue,
    succeed,
)
from twisted.internet.task import react, deferLater
from twisted.web.client import ResponseNeverReceived, Agent

from txkube import network_kubernetes_from_context

from lae_automation.subscription_manager import (
    UnexpectedResponseCode,
    network_client,
)



def cancel_subscription_main():
    """
    Find the subscription associated with ``email`` and mark it as
    de-activated in the subscription manager database (causing its resources
    to be deprovisioned).
    """
    react(cancel_subscription, argv[1:])



def cancel_subscription(reactor, k8s_context, email):
    return _with_subscription_manager(
        reactor, k8s_context,
        partial(_cancel_one_subscription, email),
    )



@inlineCallbacks
def _with_subscription_manager(reactor, k8s_context, f):
    """
    Call ``f`` with a subscription manager client.

    Do this by forwarding the API port to the subscription manager pod and
    pointing a ``network_client`` at it.
    """
    print("Tunnelling to subscription manager...")
    subscription_manager_pod = yield _get_subscription_manager_pod(
        reactor, k8s_context,
    )

    # Use kubectl for the port-forward since txkube does not yet support that
    # API.
    forwarder = reactor.spawnProcess(
        ProcessProtocol(),
        b"kubectl",
        [b"kubectl",
         b"--context", k8s_context,
         b"port-forward",
         subscription_manager_pod,
         b"9009:8000",
        ],
        env=environ,
    )

    subscription_manager_client = network_client(
        b"http://127.0.0.1:9009/",
        Agent(reactor),
    )

    while True:
        try:
            yield subscription_manager_client.get("abcxyz")
        except (ConnectionRefusedError, ResponseNeverReceived):
            yield deferLater(reactor, 1.0, lambda: None)
        except UnexpectedResponseCode:
            break

    try:
        yield f(subscription_manager_client)
    finally:
        forwarder.signalProcess(b"TERM")



@inlineCallbacks
def _cancel_one_subscription(email, subscription_manager_client):
    print("Searching for subscription with matching email...")
    subscription_id = yield _get_subscription_id(subscription_manager_client, email)
    yield _cancel_one_subscription_by_id(subscription_id, subscription_manager_client)



@inlineCallbacks
def _cancel_one_subscription_by_id(subscription_id, subscription_manager_client):
    print("Canceling subscription {}...".format(subscription_id))
    yield subscription_manager_client.delete(subscription_id)
    print("Cancelled.")



@inlineCallbacks
def _get_subscription_manager_pod(reactor, k8s_context):
    k8s = network_kubernetes_from_context(reactor, k8s_context)
    k8s_client = yield k8s.versioned_client()
    podlist = yield k8s_client.list(k8s_client.model.v1.Pod)
    for pod in podlist.items:
        if _is_subscription_manager_pod(pod):
            returnValue(pod.metadata.name)
    raise Exception("Could not find subscription manager")



def _is_subscription_manager_pod(pod):
    labels = pod.metadata.labels
    return (
        pod.status.phase == "Running"
    ) and (
        labels.get("provider"), labels.get("app"), labels.get("component"),
    ) == (
        "LeastAuthority", "subscription-manager", "Infrastructure",
    )



@inlineCallbacks
def _get_subscription_id(subscription_manager_client, email):
    found = []
    subscriptions = yield subscription_manager_client.list()
    for subscription in subscriptions:
        if email == subscription.customer_email:
            found.append(subscription.subscription_id)
    if len(found) == 0:
        raise Exception("Could not find subscription with matching email")
    elif len(found) > 1:
        raise Exception(
            "Found {} subscriptions with matching email".format(len(found)),
        )
    else:
        returnValue(found[0])




def sync_subscriptions_main():
    """
    Find all subscriptions with provisioned resources but without an active
    Stripe subscription and mark them as de-activated in the subscription
    manager database (causing their resources to be deprovisioned).
    """
    react(sync_subscriptions, argv[1:])



def sync_subscriptions(reactor, k8s_context, api_key):
    return _with_subscription_manager(
        reactor, k8s_context,
        partial(_sync_subscriptions, reactor, api_key),
    )



@inlineCallbacks
def _sync_subscriptions(reactor, api_key, subscription_manager_client):
    stripe_subscriptions = yield _get_active_stripe_subscriptions(reactor, api_key)
    k8s_subscriptions = yield subscription_manager_client.list()

    for k8s_subscription in k8s_subscriptions:
        if k8s_subscription.subscription_id in stripe_subscriptions:
            continue

        yield _cancel_one_subscription_by_id(
            k8s_subscription.customer_email,
            subscription_manager_client,
        )



def _get_active_stripe_subscriptions(reactor, api_key):
    # XXX Pagination
    limit = 100
    trialinglist = stripe.Subscription.list(
        limit=limit,
        api_key=api_key,
        status="trialing",
    )
    assert not trialinglist.has_more, "You have to implement pagination now."

    activelist = stripe.Subscription.list(
        limit=limit,
        api_key=api_key,
        status="active",
    )
    assert not activelist.has_more, "You have to implement pagination now."

    return succeed(list(
        subscr.id
        for subscr
        in trialinglist.data + activelist.data
    ))
