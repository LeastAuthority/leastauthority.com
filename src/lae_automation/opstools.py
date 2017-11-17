# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module contains the implementation for command-line operations tools
with ``setup.py``-defined entrypoints.
"""

from __future__ import unicode_literals, print_function

from datetime import datetime
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



def cancel_subscription(reactor, k8s_context, email_or_subscription_id):
    return _with_subscription_manager(
        reactor, k8s_context,
        partial(_cancel_one_subscription, email_or_subscription_id.decode("utf-8")),
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
def _cancel_one_subscription(email_or_subscription_id, subscription_manager_client):
    print("Searching for subscription with matching email...")
    if "@" in email_or_subscription_id:
        email = email_or_subscription_id
        subscription_id = yield _get_subscription_id(subscription_manager_client, email)
    elif email_or_subscription_id.startswith("sub_"):
        subscription_id = email_or_subscription_id
    else:
        raise Exception(
            "Strange subscription identifier ({}): need an email or subscription id".format(
                email_or_subscription_id,
            ),
        )

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



def _list(collection, api_key, **kwargs):
    limit = 50
    starting_after = None
    all_items = []
    while True:
        more_items = collection.list(
            limit=limit,
            api_key=api_key,
            starting_after=starting_after,
            **kwargs
        )
        all_items.extend(more_items.data)

        if more_items.has_more:
            # Move to the next page and issue another request.
            starting_after = more_items.data[-1].id
        else:
            # All done.
            return all_items


def _get_active_stripe_subscriptions(reactor, api_key):
    """
    Get the Stripe subscriptions for which we should have S4 provisioned
    state.

    https://stripe.com/docs/subscriptions/lifecycle#states
    """
    # Subscriptions in the trial period are considered active.
    trialinglist = _list(stripe.Subscription, api_key=api_key, status="trialing")

    # Subscriptions that are past the trial period and have paid their most
    # recent invoice are considered active.
    activelist =  _list(stripe.Subscription, api_key=api_key, status="active")

    # Subscriptions which have failed an initial billing attempt are also
    # considered active because a subsequent billing attempt will still be
    # made (automatically) and may succeed.
    pastduelist = _list(stripe.Subscription, api_key=api_key, status="past_due")

    # canceled and unpaid subscriptions are not considered active.  They must
    # be returned to some other state in order to be allowed resources in our
    # system.

    return succeed(list(
        subscr.id
        for subscr
        in trialinglist + activelist + pastduelist
    ))



def copy_subscriptions_to_account():
    _copy_subscriptions_to_account(argv[1], argv[2])



def _copy_subscriptions_to_account(source, destination):
    """
    Given API keys to two Stripe accounts holding the same Customers and Credit
    Cards, create Plans and Subscriptions in the latter account which match
    those in the former account.
    """
    source_customers = _list(stripe.Customer, api_key=source)
    print("Found {} customers in {}...".format(len(source_customers), source))
    for customer in source_customers:
        _copy_customer_subscriptions(customer, destination)



def _copy_customer_subscriptions(customer, destination):
    """
    Copy subscriptions belonging to ``customer`` to a Stripe account accessed
    via API key ``destination``.
    """
    for subscription in customer.subscriptions:
        _copy_customer_subscription(customer, subscription, destination)
        _terminate_customer_subscription(customer, subscription)



def _copy_customer_subscription(customer, subscription, destination):
    """
    Copy one subscription belonging to ``customer`` to a Stripe account
    accessed via API key ``destination``.
    """
    trial_end = subscription.current_period_end
    plans = list(
        subscription_item["plan"]["id"]
        for subscription_item
        in subscription["items"]["data"]
    )
    print("Copying customer {} subscription {} (trialing until {}) with plans {}".format(
        customer.id,
        subscription.id,
        datetime.utcfromtimestamp(trial_end).isoformat(),
        plans,
    ))
    stripe.Subscription.create(
        api_key=destination,
        customer=customer.id,
        trial_end=trial_end,
        items=list(
            {"plan": plan}
            for plan
            in plans
        ),
    )



def _terminate_customer_subscription(customer, subscription):
    """
    Terminate one subscription at the end of its current billing period.
    """
    print("Terminating customer {} origin subscription {} at end of period ({}).".format(
        customer.id,
        subscription.id,
        datetime.utcfromtimestamp(subscription.current_period_end).isoformat(),
    ))
    subscription.delete(at_period_end=True)
