# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module provides functionality for creating new subscriptions
("signing up") and conveying the subscription details to the subscription
owner.
"""

from base64 import b32encode, b64encode
import json
from datetime import datetime

from zope.interface import Interface, implementer

import attr
from attr import validators

from eliot import start_action
from eliot.twisted import DeferredContext

from twisted.python.monkey import MonkeyPatcher
from twisted.python.filepath import FilePath
from twisted.internet.defer import Deferred, succeed
from twisted.web.client import Agent

from wormhole import xfer_util

from lae_automation.model import SubscriptionDetails

from .subscription_manager import Client, network_client

SIGNUP_ICON_PATH = FilePath(__file__).sibling(u"lae-s4-signup-icon.png")
SIGNUP_ICON_BASE64 = b64encode(SIGNUP_ICON_PATH.getContent())


def encode_id(ident):
    return b32encode(ident).rstrip("=").lower()


def get_bucket_name(subscription_id, customer_id):
    return "lae-%s-%s" % (encode_id(subscription_id), encode_id(customer_id))


def provision_subscription(smclient, details):
    """
    Create the subscription state in the SubscriptionManager service.

    :param SubscriptionDetails details:
    """
    def created(details):
        d = _wait_for_service(details.subscription_id)
        d.addCallback(lambda ignored: details)
        return d

    a = start_action(action_type=u"signup:provision-subscription")
    with a.context():
        d = DeferredContext(
            smclient.create(details.subscription_id, details),
        )
        d.addCallback(created)
        return d.addActionFinish()



def _wait_for_service(subscription_id):
    # XXX Poll Kubernetes or DNS or something looking for matching resources.
    # XXX With a timeout and some error logging.
    return succeed(None)



class ISignup(Interface):
    def signup(customer_email, customer_id, subscription_id, plan_id):
        """
        Create a new subscription with the given details.

        :param unicode customer_email: An email address to associate with the
            subscription.

        :param unicode customer_id: A unique identifier associated with the
            customer associated with thi s subscription.

        :param unicode subscription_id: A unique identifier to associate with
            the newly created subscription.

        :param unicode plan_id: The identifier of the service plan to which

        :return Deferred(IClaim): An object detailing how the resources
            provisioned for the subscription can be claimed.
        """



class IClaim(Interface):
    def describe():
        """
        Explain the details of this claim in a way that can be rendered into a web
        page.

        :return: XXX How do we put stuff in web pages?  Jinja2?
            twisted.web.template?
        """



@attr.s(frozen=True)
class _Provisioner(object):
    smclient = attr.ib(validator=validators.instance_of(Client))
    provision_subscription = attr.ib()

    def signup(self, customer_email, customer_id, subscription_id, plan_id):
        details = SubscriptionDetails(
            bucketname=get_bucket_name(subscription_id, customer_id),
            oldsecrets=None,
            customer_email=customer_email,
            customer_pgpinfo="",
            product_id=plan_id,
            customer_id=customer_id,
            subscription_id=subscription_id,
            introducer_port_number=0,
            storage_port_number=0,
        )
        a = start_action(action_type=u"provisioning-signup")
        with a.context():
            d = DeferredContext(
                self.provision_subscription(
                    self.smclient, details,
                ),
            )
            return d.addActionFinish()



@implementer(ISignup)
@attr.s(frozen=True)
class _EmailSignup(object):
    reactor = attr.ib()
    provisioner = attr.ib()

    send_signup_confirmation = attr.ib()
    send_notify_failure = attr.ib()

    def signup(self, customer_email, customer_id, subscription_id, plan_id):
        d = self.provisioner.signup(customer_email, customer_id, subscription_id, plan_id)
        d.addCallback(self._notify_success)
        d.addCallback(lambda ignored: _EmailClaim())
        d.addErrback(self._notify_failure, customer_email, customer_id, subscription_id, plan_id)
        return d


    def _notify_success(self, details):
        from sys import stdout, stderr
        a = start_action(
            action_type=u"signup:send-confirmation",
            subscription=attr.asdict(details),
        )
        with a.context():
            d = DeferredContext(self.send_signup_confirmation(
                details.customer_email, details.external_introducer_furl,
                None, stdout, stderr,
            ))
            return d.addActionFinish()


    def _notify_failure(self, reason, customer_email, customer_id, subscription_id, plan_id):
        from sys import stdout, stderr
        # XXX Eliot log reason here too
        a = start_action(action_type=u"signup:send-failure")
        with a.context():
            d = DeferredContext(self.send_notify_failure(
                reason, customer_email, None,
                stdout, stderr,
            ))
            return d.addActionFinish()


@implementer(IClaim)
class _EmailClaim(object):
    description = (
        u"We'll send you an email with your unique introducer furl within the next hour."
    )
    def describe(self):
        return self.description



def get_provisioner(reactor, subscription_manager_endpoint, provision_subscription):
    endpoint = subscription_manager_endpoint.asText().encode("utf-8")
    agent = Agent(reactor)
    smclient = network_client(endpoint, agent)
    return _Provisioner(smclient, provision_subscription)



def get_email_signup(reactor, provisioner, send_signup_confirmation, send_notify_failure):
    return _EmailSignup(
        reactor,
        provisioner,
        send_signup_confirmation,
        send_notify_failure,
    )



def get_wormhole_signup(reactor, provisioner, wormhole, rendezvous_url, result_path):
    """
    Get an ``ISignup`` which conveys subscription details to the subscriber by
    sending them through a magic wormhole.

    :param provisioner: The thing which can create a new subscription when a
        new user signs up.  See ``get_provisioner``.

    :param wormhole: An object like ``wormhole.wormhole`` to use to create
        wormholes.

    :param URL rendezvous_url: The location of the magic wormhole rendezvous
        server.

    :param FilePath result_path: The location of a file to which
        wormhole-related results will be written.

    :return: An ``ISignup`` provider.
    """
    return _WormholeSignup(
        reactor,
        provisioner,
        wormhole,
        rendezvous_url,
        result_path,
    )



@implementer(ISignup)
@attr.s(frozen=True)
class _WormholeSignup(object):
    """
    An ``ISignup`` that puts a Tahoe-LAFS configuration blob into a magic
    wormhole and publishes the wormhole code for receiving it.

    :ivar provisioner: See ``get_wormhole_signup``.

    :ivar wormhole: See ``get_wormhole_signup``.

    :ivar rendezvous_url: See ``get_wormhole_signup``.

    :ivar result_path: See ``get_wormhole_signup``.
    """
    reactor = attr.ib()
    provisioner = attr.ib()
    wormhole = attr.ib()
    rendezvous_url = attr.ib()
    result_path = attr.ib()

    def signup(self, *args, **kwargs):
        """
        Provision a subscription and return an ``IClaim`` describing how to
        retrieve the resulting configuration from a magic wormhole server.
        """
        a = start_action(action_type=u"wormhole-signup")
        with a.context():
            d = DeferredContext(self.provisioner.signup(*args, **kwargs))
            d.addCallback(self._details_to_wormhole_code)
            return d.addActionFinish()


    def _details_to_wormhole_code(self, details):
        """
        Put the configuration details for a subscription into a magic wormhole and
        return a ``Deferred`` that fires with the wormhole code.
        """
        configuration = _details_to_tahoe_configuration(details)
        wormhole_code, done = _configuration_to_wormhole_code(
            self.reactor,
            self.wormhole,
            self.rendezvous_url,
            configuration,
        )

        done.addCallback(_wormhole_claimed, self.reactor, details)
        done.addErrback(_wormhole_failed, self.reactor, details)
        done.addCallback(_record_result, self.result_path)
        return wormhole_code



def _wormhole_claimed(ignored, reactor, details):
    return {
        u"claim": u"claimed",
        u"subscription-id": details.subscription_id,
        u"timestamp": datetime.utcfromtimestamp(reactor.seconds()).isoformat(),
    }


def _wormhole_failed(error, reactor, details):
    return {
        u"claim": u"failed", u"subscription-id": details.subscription_id,
        u"error": error.getTraceback(),
        u"timestamp": datetime.utcfromtimestamp(reactor.seconds()).isoformat(),
    }


def _record_result(result, result_path):
    with result_path.open("at") as fObj:
        fObj.write(json.dumps(result) + u"\n")


def _details_to_tahoe_configuration(details):
    return {
        u"version": 1,
        u"nickname": u"Least Authority S4",
        u"introducer": details.external_introducer_furl,
        u"shares-needed": u"1",
        u"shares-total": u"1",
        u"shares-happy": u"1",
        u"icon_base64": SIGNUP_ICON_BASE64,
    }



def _configuration_to_wormhole_code(reactor, wormhole, rendezvous_url, configuration):
    """
    Serialize ``configuration`` to JSON and put it into a new wormhole created
    at the server given by ``rendezvous_url``.

    :return (Deferred(IClaim), Deferred): Two Deferreds representing two
        different events.  The first represents the creation of the magic
        wormhole.  It fires with a claim that describes the wormhole from
        which the configuration can be retrieved.  The second represents the
        complete transfer of the configuration to the client.  It fires with a
        meaningless success result or a ``Failure`` if something is known to
        have gone wrong.
    """
    waiting_for_code = Deferred()

    def got_code(code):
        # We don't currently expire these at all.
        expires = _NoExpiration()
        waiting_for_code.callback(_WormholeClaim(code, expires))

    patcher = MonkeyPatcher()
    patcher.addPatch(xfer_util, "wormhole", wormhole)
    patcher.patch()
    try:
        done = xfer_util.send(
            reactor,
            # This has to agree with anyone who wants to receive this code.  "v1"
            # here versions application protocol that runs over wormhole to convey
            # a configuration payload.  It does not version the configuration
            # payload itself, which is a self-versioning JSON.
            appid=u"tahoe-lafs.org/tahoe-lafs/v1",
            relay_url=rendezvous_url.asText(),
            data=json.dumps(configuration),
            code=None,
            use_tor=None,
            on_code=got_code,
        )
    finally:
        patcher.restore()

    return waiting_for_code, done


class _NoExpiration(object):
    def __unicode__(self):
        return u""


class _TimeBasedExpiration(object):
    when = attr.ib(validator=validators.instance_of(datetime))

    def __unicode__(self):
        return u", expires {}".format(self.when.isoformat())


@implementer(IClaim)
@attr.s(frozen=True)
class _WormholeClaim(object):
    code = attr.ib(validator=validators.instance_of(unicode))
    expires = attr.ib()

    def describe(self):
        return self.code + unicode(self.expires)
