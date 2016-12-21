"""
Tests for ``lae_automation.subscription_manager``.
"""

from tempfile import mkdtemp

from twisted.python.filepath import FilePath

from lae_automation.subscription_manager import memory_client

from lae_util.testtools import TestCase

from testtools.matchers import Equals, Is, HasLength

from hypothesis import given, assume

from .strategies import subscription_id, furl, bucket_name

class SubscriptionManagerTestMixin(object):
    """
    Tests for the subscription manager's HTTP interface.
    """
    def get_client(self):
        raise NotImplementedError()

    @given(subscription_id(), furl(), bucket_name())
    def test_round_trip(self, subscription_id, introducer_furl, bucket_name):
        """
        A subscription created with a PUT to /v1/subscriptions/<id> can be
        retrieved with a GET of /v1/subscriptions.
        """
        client = self.get_client()
        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        d = client.create(subscription_id, details)
        self.expectThat(self.successResultOf(d), Is(None))

        d = client.list()
        subscriptions = self.successResultOf(d)
        self.expectThat(subscriptions, Equals([subscription_id]))

        d = client.get(subscription_id)
        subscription = self.successResultOf(d)
        expected = details.copy()
        expected["id"] = subscription_id
        expected["active"] = True
        del subscription["details"]["introducer_port"]
        del subscription["details"]["storage_port"]
        self.expectThat(subscription, Equals(dict(version=1, details=expected)))

    @given(subscription_id(), subscription_id(), furl(), bucket_name())
    def test_port_assignment(self, id_a, id_b, introducer_furl, bucket_name):
        """
        An unused port pair is assigned to new subscriptions.
        """
        assume(id_a != id_b)
        client = self.get_client()
        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        self.successResultOf(client.create(id_a, details))
        self.successResultOf(client.create(id_b, details))

        details_a = self.successResultOf(client.get(id_a))
        details_b = self.successResultOf(client.get(id_b))

        ports = {
            details_a["details"]["introducer_port"],
            details_a["details"]["storage_port"],
            details_b["details"]["introducer_port"],
            details_b["details"]["storage_port"],
        }
        self.expectThat(ports, HasLength(4))

    @given(subscription_id(), furl(), bucket_name())
    def test_deactivate_subscription(self, subscription_id, introducer_furl, bucket_name):
        """
        A DELETE to /v1/subscriptions/<id> causes a subscription to be
        deactivated such that it is no longer included in the result
        of a GET to /v1/subscriptions.
        """
        client = self.get_client()
        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        self.successResultOf(client.create(subscription_id, details))
        self.successResultOf(client.delete(subscription_id))

        subscriptions = self.successResultOf(client.list())
        self.assertThat(subscriptions, Equals([]))

class SubscriptionManagerTests(SubscriptionManagerTestMixin, TestCase):
    def get_client(self):
        return memory_client(FilePath(mkdtemp().decode("utf-8")))
