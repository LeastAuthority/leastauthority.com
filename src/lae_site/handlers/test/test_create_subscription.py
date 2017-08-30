"""
Tests for ``lae_site.create_subscription``.
"""

import attr

from testtools.matchers import Equals, Contains

from twisted.internet.defer import succeed
from twisted.web.client import readBody
from twisted.web.http import OK
from twisted.web.resource import Resource

from lae_util.testtools import TestCase
from lae_site.handlers.create_subscription import CreateSubscription

from treq.testing import RequestTraversalAgent


@attr.s
class TrivialClaim(object):
    email = attr.ib()
    customer_id = attr.ib()
    subscription_id = attr.ib()
    plan_id = attr.ib()

    def describe(self, env):
        return "You subscribed."



class TrivialSignup(object):
    def signup(self, email, customer_id, subscription_id, plan_id):
        return succeed(
            TrivialClaim(email, customer_id, subscription_id, plan_id),
        )



@attr.s
class Customer(object):
    id = attr.ib()
    email = attr.ib()
    subscriptions = attr.ib()



@attr.s
class Subscriptions(object):
    data = attr.ib()


@attr.s
class Subscription(object):
    id = attr.ib()
    plan = attr.ib()



@attr.s
class Plan(object):
    id = attr.ib()


@attr.s
class PositiveStripe(object):
    def create(self, authorization_token, plan_id, email):
        return Customer(
            "cus_abcdef",
            email, Subscriptions([
                Subscription("sub_123456", Plan(plan_id)),
            ]),
        )


@attr.s
class Mail(object):
    from_addr = attr.ib()
    to_addr = attr.ib()
    subject = attr.ib()
    headers = attr.ib()



@attr.s
class MemoryMailer(object):
    emails = attr.ib(default=attr.Factory(list))

    def mail(self, from_addr, to_addr, subject, headers):
        self.emails.append(Mail(
            from_addr, to_addr, subject, headers,
        ))



class FullSignupTests(TestCase):
    """
    Tests for ``CreateSubscription``.
    """
    def test_render_signup_success(self):
        """
        """
        self.signup = TrivialSignup()
        self.mailer = MemoryMailer()
        self.stripe = PositiveStripe()
        self.cross_domain = "http://localhost:5000/"

        resource = CreateSubscription(
            lambda style: self.signup, self.mailer, self.stripe, self.cross_domain
        )
        root = Resource()
        root.putChild(b"", resource)

        agent = RequestTraversalAgent(root)
        d = agent.request(b"POST", b"http://127.0.0.1/?stripeToken=abc&email=alice@example.invalid")
        response = self.successResultOf(d)
        body = self.successResultOf(readBody(response))

        self.expectThat(response.code, Equals(OK))
        self.expectThat(body, Contains("You subscribed."))
        self.expectThat(self.mailer.emails, Equals([]))
