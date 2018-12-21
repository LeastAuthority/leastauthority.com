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
import chargebee

from twisted.internet.error import ConnectionRefusedError
from twisted.internet.protocol import ProcessProtocol
from twisted.internet.defer import (
    inlineCallbacks,
    returnValue,
    succeed,
)
from twisted.internet.task import react, deferLater
from twisted.web.client import ResponseNeverReceived, Agent, readBody

from txkube import (
    KubernetesError,
    network_kubernetes_from_context,
)

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
    subscription_manager_pod = (yield _get_subscription_manager_pod(
        reactor, k8s_context,
    )).metadata.name

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
def _get_subscription_id(subscription_manager_client, email_or_subscription_id):
    if "@" not in email_or_subscription_id:
        returnValue(email_or_subscription_id)

    email = email_or_subscription_id
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


@inlineCallbacks
def _cancel_one_subscription(email_or_subscription_id, subscription_manager_client):
    print("Searching for subscription with matching email...")
    subscription_id = yield _get_subscription_id(subscription_manager_client, email_or_subscription_id)
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
            returnValue(pod)
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
    raw_input("Found {} customers in {}...".format(len(source_customers), source))
    destination_subscriptions = _list(stripe.Subscription, api_key=destination)
    raw_input("Found {} subscriptions in {}...".format(len(destination_subscriptions), destination))
    for customer in source_customers:
        _copy_customer_subscriptions(destination_subscriptions, customer, destination)



def _copy_customer_subscriptions(destination_subscriptions, customer, destination):
    """
    Copy subscriptions belonging to ``customer`` to a Stripe account accessed
    via API key ``destination``.
    """
    for subscription in customer.subscriptions:
        _copy_customer_subscription(destination_subscriptions, customer, subscription, destination)
        _terminate_customer_subscription(customer, subscription, False)


PLAN_MAPPING = {
    "S4_2014_07_10_Complimentary_Service": "S4_comp",
    "S4_consumer_iteration_2_beta1_2014-05-27": "S4_consumer_iteration_2_beta1_2014-05-27",
}


def _copy_customer_subscription(destination_subscriptions, customer, subscription, destination):
    """
    Copy one subscription belonging to ``customer`` to a Stripe account
    accessed via API key ``destination``.
    """
    trial_end = subscription.current_period_end
    desired_plans = {
        PLAN_MAPPING[subscription_item["plan"]["id"]]
        for subscription_item
        in subscription["items"]["data"]
    }
    # Check to see if it was already copied by a previous run.
    for dest_subscription in destination_subscriptions:
        if dest_subscription["customer"] == customer.id:
            observed_plans = {
                item["plan"]["id"]
                for item
                in dest_subscription["items"]["data"]
            }
            if observed_plans != desired_plans:
                raw_input(
                    "Skewed subscription detected for {}!!!\n"
                    "Desired plans: {}\n"
                    "Observed plans: {}\n".format(
                        customer.id,
                        desired_plans,
                        observed_plans

                    ))
                raise SystemExit(1)
            if subscription["cancel_at_period_end"] and not dest_subscription["cancel_at_period_end"]:
                raw_input("Marking already-migrated subscription as cancel-at-period-end to match source.")
                _terminate_customer_subscription(customer, dest_subscription, True)
                return

            else:
                raw_input("Already-migrated subscription detected for {}, skipping.".format(customer.id))
                return

    raw_input(
        "Copying customer\n"
        "Customer: {} {}\n"
        "Subscription: {}\n"
        "Trialing until: {}".format(
            customer.id, customer.email,
            subscription,
            datetime.utcfromtimestamp(trial_end).isoformat(),
        )
    )
    new_subscription = stripe.Subscription.create(
        api_key=destination,
        customer=customer.id,
        trial_end=trial_end,
        items=list(
            {"plan": plan}
            for plan
            in desired_plans
        ),
    )
    print("Copy: {}\n".format(new_subscription))
    if subscription["status"] == "canceled":
        raw_input("Subscription was cancelled, cancelling copy.")
        _terminate_customer_subscription(customer, new_subscription, False)
    elif subscription["cancel_at_period_end"]:
        raw_input("Subscription was cancelled, cancelling copy.")
        _terminate_customer_subscription(customer, new_subscription, True)


def _terminate_customer_subscription(customer, subscription, at_period_end):
    """
    Terminate one subscription at the end of its current billing period.
    """
    print("Terminating customer {} subscription {} {}.".format(
        customer.id,
        subscription.id,
        "at period end ({})".format(
            datetime.utcfromtimestamp(
                subscription.current_period_end,
            ).isoformat(),
        )
        if at_period_end
        else "now"
    ))
    subscription.delete(at_period_end=at_period_end)
    with open("terminate.log", "a") as f:
        f.write("{},{},{}\n".format(customer.id, subscription.id, at_period_end))


def move_stripe_subscriptions_to_chargebee():
    _move_stripe_subscriptions_to_chargebee(*argv[1:4])


def _move_stripe_subscriptions_to_chargebee(stripe_key, chargebee_key, chargebee_site):
    chargebee.configure(chargebee_key, chargebee_site)

    print("Listing Stripe customers...")
    stripe_customers = _list(stripe.Customer, api_key=stripe_key)
    print("Done.")
    _move_customer_subscriptions_to_chargebee(stripe_customers)


def _move_customer_subscriptions_to_chargebee(stripe_customers):
    for stripe_customer in stripe_customers:
        if not stripe_customer.email:
            print("Empty email for customer {}, not processing.".format(stripe_customer.id))
            with open("empty-emails.log", "a") as f:
                f.write("{}\n".format(stripe_customer.id))
            continue
        print("Copying Stripe customer to Chargebee: {} ...".format(stripe_customer.email))
        try:
            chargebee_customer = _copy_customer_to_chargebee(stripe_customer)
        except Exception as e:
            print("Copying Stripe customer {} failed:".format(stripe_customer.id))
            print(e)
            with open("customer-copy-errors.log", "a") as f:
                f.write("{},{}\n".format(stripe_customer.id, str(e)))
            continue

        print("Done.  Chargebee customer {}".format(chargebee_customer.id))
        with open("customer-copies.log", "a") as f:
            f.write("{},{}\n".format(stripe_customer.id, chargebee_customer.id))
        for stripe_subscription in stripe_customer.subscriptions:
            print("Moving Stripe subscriptions to Chargebee...")
            _move_customer_subscription_to_chargebee(stripe_customer, stripe_subscription, chargebee_customer)
            print("Done")


def _copy_customer_to_chargebee(stripe_customer):
    # Find an existing customer with a matching email if we can
    entries = chargebee.Customer.list({
        "email[is]": stripe_customer.email,
    })
    for entry in entries:
        print("Using existing customer")
        return entry.customer
    print("Creating new customer")
    result = chargebee.Customer.create({
        "email": stripe_customer.email,
    })
    return result.customer


PLAN_MAP = {
    "S4_comp": "s4_250gb_complementary",
    "plan_DR6FlcWNrX0eXs": "s4_250gb_complementary",
}

def _move_customer_subscription_to_chargebee(stripe_customer, stripe_subscription, chargebee_customer):
    plan_id = PLAN_MAP.get(
        stripe_subscription.plan.id,
        stripe_subscription.plan.id,
    )
    staging_stripe_gateway = "gw_B4eONuQrfSi2IZGq"
    production_stripe_gateway = "gw_2smoc9C8Qz9QjPL1CT"

    trial_end = stripe_subscription.current_period_end
    if stripe_customer.sources.data:
        print("Customer has payment sources: {}".format(stripe_customer.email))
        result = chargebee.Customer.update_payment_method(
            chargebee_customer.id, {
                "payment_method": {
                    "type": "card",
                    "gateway_account_id": production_stripe_gateway,
                    "reference_id": "{}/{}".format(stripe_customer.id, stripe_customer.sources.data[0].id),
                },
            },
        )
        payment_source_id = result.customer.primary_payment_source_id
    else:
        print("Customer has no payment sources: {}".format(stripe_customer.email))
        payment_source_id = None

    with open("payment-sources.log", "a") as f:
        f.write("{},{},{}\n".format(stripe_customer.id, chargebee_customer.id, payment_source_id))
    print("Creating Chargebee subscription for customer {} ...".format(chargebee_customer.id))
    result = chargebee.Subscription.create_for_customer(
        chargebee_customer.id, {
            "plan_id": plan_id,
            "trial_end": trial_end,
            "payment_source_id": payment_source_id,
        },
    )
    with open("moved-subscriptions.log", "a") as f:
        f.write("{},{},{},{},{},{},{}\n".format(stripe_customer.id, stripe_subscription.id, stripe_subscription.plan.id, chargebee_customer.id, result.subscription.id, plan_id, stripe_customer.email))

    print("Terminating Stripe subscription for {}: {}".format(stripe_customer.email, stripe_subscription.id))
    _terminate_customer_subscription(stripe_customer, stripe_subscription, False)
    raw_input("Done")


def reinvite_customer():
    k8s_context = argv[1].decode("utf-8")
    email_or_subscription_id = argv[2].decode("utf-8")
    react(
        lambda reactor: _with_subscription_manager(
            reactor,
            k8s_context,
            partial(
                _reinvite_customer,
                reactor=reactor,
                k8s_context=k8s_context,
                email_or_subscription_id=email_or_subscription_id,
            ),
        ),
    )

@inlineCallbacks
def _reinvite_customer(subscription_manager_client, reactor, k8s_context, email_or_subscription_id):
    # Try to create a new re-invite pod for the given subscription.
    # If this fails because one already exists, proceed.
    # Look at the re-invite pod to extract the wormhole code and spit it out.
    kubernetes = network_kubernetes_from_context(reactor, k8s_context)
    client = yield kubernetes.versioned_client()
    subscription_id = yield _get_subscription_id(subscription_manager_client, email_or_subscription_id)
    try:
        yield _create_reinvite_pod(reactor, k8s_context, subscription_id, client)
    except KubernetesError as e:
        if e.code != 409:
            # If it is anything other than "pod already exists", propagate it.
            raise

    invite_code = yield _get_invite_code(subscription_id, reactor, client)
    print("Invite code: {}".format(invite_code))


@inlineCallbacks
def _create_reinvite_pod(reactor, k8s_context, subscription_id, client):
    print("Determining reinvite image...")
    subscription_manager_pod = yield _get_subscription_manager_pod(
        reactor, k8s_context,
    )
    image = subscription_manager_pod.spec.containers[0].image

    v1 = client.model.v1
    print("Creating reinvite pod...")
    creating = yield client.create(v1.Pod(
        metadata=v1.ObjectMeta(
            namespace=u"default",
            name=_reinvite_pod_name(subscription_id),
        ),
        spec=v1.PodSpec(
            restartPolicy=u"OnFailure",
            containers=[v1.Container(
                name=_reinvite_pod_name(subscription_id),
                image=image,
                args=[u"/app/env/bin/python", u"-c", REINVITE_CODE, subscription_id],
            )],
        ),
    ))
    print(creating)

REINVITE_CODE = u"""
from lae_automation.opstools import _reinvite_server
_reinvite_server()
"""

def _reinvite_server():
    react(_reinvite_server2, argv[1:])


@inlineCallbacks
def _reinvite_server2(reactor, subscription_id):
    provisioner = None
    subscription_id = subscription_id.decode("utf-8")
    subscription_manager_client = network_client(
        b"http://subscription-mananger/",
        Agent(reactor),
    )
    details = yield subscription_manager_client.get(subscription_id)
    from lae_automation.signup import get_wormhole_signup
    from wormhole import wormhole
    from hyperlink import URL
    from twisted.python.filepath import FilePath
    from tempfile import mktemp
    results_path = FilePath(mktemp())
    rendezvous_url = URL.fromText(u"ws://wormhole:4000/v1")
    signup = get_wormhole_signup(
        reactor,
        provisioner,
        wormhole,
        rendezvous_url,
        results_path,
    )
    wormhole_code = yield signup._details_to_wormhole_code(details)
    print("invite-code: {}".format(wormhole_code))
    while not (results_path.exists() and results_path.getsize()):
        yield deferLater(reactor, 30.0, lambda: None)


@inlineCallbacks
def _get_invite_code(subscription_id, reactor, client):
    which = client.model.v1.Pod(
        metadata=client.model.v1.ObjectMeta(
            namespace=u"default",
            name=_reinvite_pod_name(subscription_id),
        ),
    )
    print("Waiting for running pod...")
    while True:
        pod = yield client.get(which)
        if pod.status.phase == u"Running":
            break
        if pod.status.phase == u"Failed":
            break
        print("...Pod status is {}".format(pod.status.phase))
        yield deferLater(reactor, 1.0, lambda: None)


    print("Waiting for complete log...")
    while True:
        log = yield pod_log(client, u"default", _reinvite_pod_name(subscription_id))
        print("Log: {}".format(log))
        if "invite-code:" in log:
            break
        pod = yield client.get(which)
        if pod.status.phase != u"Running":
            break
        yield deferLater(reactor, 1.0, lambda: None)

    invite_code_line = list(line for line in log.splitlines() if "invite-code:" in line)[0]
    invite_code = invite_code_line.split("invite-code:")[1].split()[0]
    returnValue(invite_code)


def _reinvite_pod_name(subscription_id):
    return u"reinvite-{}".format(subscription_id.encode("base64").lower().strip().strip(u"="))


def pod_log(client, pod_namespace, pod_name):
    url = client.kubernetes.base_url.child(
        u"api",
        u"v1",
        u"namespaces",
        pod_namespace,
        u"pods",
        pod_name,
        u"log",
    )
    print("Getting {}".format(url))
    d = client._get(url)
    d.addCallback(readBody)
    return d
