# Copyright Least Authority Enterprises.
# See LICENSE for details.

from base64 import b32encode, b64encode
import json
from functools import partial
from datetime import datetime

from zope.interface import Interface, implementer

import attr
from attr import validators

from eliot import start_action
from eliot.twisted import DeferredContext

from twisted.python.filepath import FilePath
from twisted.internet.defer import Deferred, succeed
from twisted.internet import reactor
from twisted.web.client import Agent

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



def get_signup(reactor, provisioner, send_signup_confirmation, send_notify_failure):
    return _EmailSignup(
        reactor,
        provisioner,
        send_signup_confirmation,
        send_notify_failure,
    )



@implementer(ISignup)
@attr.s(frozen=True)
class _WormholeSignup(object):
    reactor = attr.ib()
    basic_signup = attr.ib(validator=validators.provides(ISignup))

    def signup(self, *args, **kwargs):
        a = start_action(action_type=u"wormhole-signup")
        with a.context():
            d = DeferredContext(self.basic_signup.signup(*args, **kwargs))
            d.addCallback(_claim_to_tahoe_configuration)
            d.addCallback(partial(_configuration_to_wormhole_code, reactor))
            return d.addActionFinish()



def _claim_to_tahoe_configuration(claim):
    details = claim.details
    return {
        u"version": 1,
        u"nickname": u"Least Authority S4",
        u"introducer": details.external_introducer_furl,
        u"shares-needed": u"1",
        u"shares-total": u"1",
        u"shares-happy": u"1",
        u"icon_base64": SIGNUP_ICON_BASE64,
    }



def _configuration_to_wormhole_code(reactor, configuration):
    # Put it into a new wormhole
    # Return a _WormholeClaim with the new wormhole's code.
    from wormhole.xfer_util import send

    waiting_for_code = Deferred()

    def got_code(code):
        print("Huzzah", code)
        waiting_for_code.callback(_WormholeClaim(code, datetime.now()))

    print("!Hazzuh")
    done = send(
        reactor,
        # This has to agree with anyone who wants to receive this code.
        appid=u"tahoe-lafs.org/tahoe-lafs/v1",
        relay_url=u"ws://wormhole.leastauthority.com:4000/v1",
        data=json.dumps(configuration),
        code=None,
        use_tor=None,
        on_code=got_code,
    )
    done.addBoth(record_wormhole_claim)

    return waiting_for_code



def record_wormhole_claim(result):
    print("Wormhole:", result)



@implementer(IClaim)
@attr.s(frozen=True)
class _WormholeClaim(object):
    code = attr.ib(validator=validators.instance_of(unicode))
    expires = attr.ib(validator=validators.instance_of(datetime))

    def describe(self):
        return u"{}, expires {}".format(self.code, self.expires.isoformat())
