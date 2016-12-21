"""
Tests for ``lae_automation.subscription_manager``.
"""

from tempfile import mkdtemp

import attr

from twisted.python.filepath import FilePath

from testtools.matchers import Equals, Is, HasLength

from hypothesis import given, assume

from lae_automation.subscription_manager import memory_client

from lae_util.testtools import TestCase

from .strategies import subscription_id, subscription_details

class SubscriptionManagerTestMixin(object):
    """
    Tests for the subscription manager's HTTP interface.
    """
    def get_client(self):
        raise NotImplementedError()

    @given(subscription_id(), subscription_details())
    def test_round_trip(self, subscription_id, details):
        """
        A subscription created with a PUT to /v1/subscriptions/<id> can be
        retrieved with a GET of /v1/subscriptions.
        """
        client = self.get_client()
        d = client.create(subscription_id, details)
        self.expectThat(self.successResultOf(d), Is(None))

        subscriptions = self.successResultOf(client.list())
        self.expectThat(subscriptions, Equals([subscription_id]))

        subscription = self.successResultOf(client.get(subscription_id))

        # Ports are randomly assigned, don't bother comparing them.
        expected = attr.assoc(
            details,
            introducer_port_number=0,
            storage_port_number=0,
        )
        subscription = attr.assoc(
            subscription,
            introducer_port_number=0,
            storage_port_number=0,
        )            
        self.expectThat(attr.asdict(expected), Equals(attr.asdict(subscription)))

    @given(subscription_id(), subscription_id(), subscription_details())
    def test_port_assignment(self, id_a, id_b, details):
        """
        An unused port pair is assigned to new subscriptions.
        """
        assume(id_a != id_b)
        client = self.get_client()
        self.successResultOf(client.create(id_a, details))
        self.successResultOf(client.create(id_b, details))

        details_a = self.successResultOf(client.get(id_a))
        details_b = self.successResultOf(client.get(id_b))

        ports = {
            details_a.introducer_port_number,
            details_a.storage_port_number,
            details_b.introducer_port_number,
            details_b.storage_port_number,
        }
        self.expectThat(ports, HasLength(4))

    @given(subscription_id(), subscription_details())
    def test_deactivate_subscription(self, subscription_id, details):
        """
        A DELETE to /v1/subscriptions/<id> causes a subscription to be
        deactivated such that it is no longer included in the result
        of a GET to /v1/subscriptions.
        """
        client = self.get_client()
        self.successResultOf(client.create(subscription_id, details))
        self.successResultOf(client.delete(subscription_id))

        subscriptions = self.successResultOf(client.list())
        self.assertThat(subscriptions, Equals([]))

class SubscriptionManagerTests(SubscriptionManagerTestMixin, TestCase):
    def get_client(self):
        return memory_client(FilePath(mkdtemp().decode("utf-8")))
