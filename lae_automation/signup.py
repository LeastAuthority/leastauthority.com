# Copyright Least Authority Enterprises.
# See LICENSE for details.

from base64 import b32encode

from zope.interface import Interface, implementer

import attr
from attr import validators

from eliot import start_action
from eliot.twisted import DeferredContext

from twisted.internet.defer import succeed
from twisted.web.client import Agent

from lae_automation.model import SubscriptionDetails

from .subscription_manager import Client, network_client


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
