"""
Tests for ``lae_site.create_subscription``.
"""

from json import dumps

import attr

from chargebee import PaymentError

from testtools.matchers import (
    Equals,
    Contains,
    HasLength,
    raises
)

from twisted.internet.defer import succeed
from twisted.web.client import readBody
from twisted.web.http import (
    OK,
    PAYMENT_REQUIRED,
)
from twisted.web.resource import Resource

from lae_util.testtools import TestCase
from lae_site.handlers.create_subscription import (
    SubscriptionResult,
    CreateSubscription,
    EUCountry,
)

from treq.testing import RequestTraversalAgent


@attr.s
class TrivialClaim(object):
    email = attr.ib()
    customer_id = attr.ib()
    subscription_id = attr.ib()
    plan_id = attr.ib()

    def describe(self, env, content_type):
        if content_type == "text/html":
            return "You subscribed."
        elif content_type == "application/json":
            return dumps({"v1": {u"success": "you subscribed."}})



class TrivialSignup(object):
    signups = 0

    def signup(self, email, customer_id, subscription_id, plan_id):
        self.signups += 1
        return succeed(
            TrivialClaim(email, customer_id, subscription_id, plan_id),
        )



@attr.s
class Customer(object):
    id = attr.ib()
    email = attr.ib()


@attr.s
class Subscriptions(object):
    data = attr.ib()



@attr.s
class Subscription(object):
    id = attr.ib()
    plan_id = attr.ib()



@attr.s
class Plan(object):
    id = attr.ib()



@attr.s
class PositiveBilling(object):
    default_plan_id = u"foo-bar"

    def create(self, authorization_token, plan_id, country, email):
        return SubscriptionResult(
            customer_email=email,
            customer_id="cus_abcdef",
            subscription_id="sub_123456",
            plan_id=plan_id,
        )


class NegativeBilling(object):
    default_plan_id = u"foo-bar"

    def create(self, authorization_token, plan_id, country, email):
        raise PaymentError(
            432, {
                "message": "Billing error",
                "error_code": 432,
            },
        )


class EUCountryTests(TestCase):
    """
    Tests for ``EUCountry``.
    """
    def test_valid(self):
        self.expectThat(
            EUCountry(b"be").country_code,
            Equals(b"be"),
        )

    def test_invalid(self):
        self.expectThat(
            lambda: EUCountry(b"xx"),
            raises(ValueError),
        )



@attr.s
class Mail(object):
    subject = attr.ib()
    headers = attr.ib()


@attr.s
class MemoryMailer(object):
    emails = attr.ib(default=attr.Factory(list))

    def mail(self, subject, headers):
        self.emails.append(Mail(
            subject, headers,
        ))



class FullSignupTests(TestCase):
    """
    Tests for ``CreateSubscription``.
    """
    def setUp(self):
        super(FullSignupTests, self).setUp()
        self.signup = TrivialSignup()
        self.mailer = MemoryMailer()
        self.billing = PositiveBilling()

    def _post(self, root, url):
        agent = RequestTraversalAgent(root)
        d = agent.request(b"POST", b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid")
        response = self.successResultOf(d)
        return response

    def test_json_render_signup_success(self):
        resource = CreateSubscription(
            lambda style: self.signup,
            self.mailer,
            self.billing,
            u"application/json",
        )
        root = Resource()
        root.putChild(b"", resource)

        response = self._post(
            root,
            b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid",
        )
        body = self.successResultOf(readBody(response))

        self.expectThat(response.code, Equals(OK))
        self.expectThat(
            body,
            Equals(dumps({"v1": {"success": "you subscribed."}})),
        )
        self.expectThat(self.mailer.emails, Equals([]))
        self.expectThat(self.signup.signups, Equals(1))

    def test_html_render_signup_success(self):
        """
        """
        resource = CreateSubscription(
            lambda style: self.signup,
            self.mailer,
            self.billing,
            u"text/html",
        )
        root = Resource()
        root.putChild(b"", resource)
        response = self._post(
            root,
            b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid",
        )
        body = self.successResultOf(readBody(response))

        self.expectThat(response.code, Equals(OK))
        self.expectThat(body, Contains("You subscribed."))
        self.expectThat(self.mailer.emails, Equals([]))
        self.expectThat(self.signup.signups, Equals(1))


    def test_html_render_signup_failure(self):
        """
        No subscription resources are provisioned if the Stripe interaction fails.
        """
        resource = CreateSubscription(
            lambda style: self.signup,
            self.mailer,
            NegativeBilling(),
            u"text/html",
        )
        root = Resource()
        root.putChild(b"", resource)

        response = self._post(
            root,
            b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid",
        )
        body = self.successResultOf(readBody(response))

        self.expectThat(response.code, Equals(PAYMENT_REQUIRED))
        self.expectThat(body, Contains("Billing error"))
        self.expectThat(self.mailer.emails, HasLength(1))
        self.expectThat(self.signup.signups, Equals(0))


    def test_json_render_signup_failure(self):
        resource = CreateSubscription(
            lambda style: self.signup,
            self.mailer,
            NegativeBilling(),
            u"application/json",
        )
        root = Resource()
        root.putChild(b"", resource)

        response = self._post(
            root,
            b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid",
        )
        body = self.successResultOf(readBody(response))

        self.expectThat(response.code, Equals(PAYMENT_REQUIRED))
        self.expectThat(body, Equals(dumps({"v1": {"error": "Billing error"}})))
        self.expectThat(self.mailer.emails, HasLength(1))
        self.expectThat(self.signup.signups, Equals(0))
