"""
Tests for ``lae_automation.subscription_manager``.
"""

from tempfile import mkdtemp
from io import BytesIO
from json import dumps, loads

from twisted.web.http import OK, CREATED
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath
from twisted.internet.defer import succeed

from lae_automation.subscription_manager import Client as SMClient, make_resource

from lae_util.testtools import TestCase

from testtools.matchers import AfterPreprocessing, Equals, Is, HasLength

from hypothesis import given, assume

from .memoryagent import MemoryAgent
from .strategies import subscription_id, furl, bucket_name

class Uncooperator(object):
    def cooperate(self, iterator):
        try:
            for ignored in iterator:
                pass
        except:
            return Uncooperator(Failure())
        else:
            return UncooperativeTask(iterator)

class UncooperativeTask(object):
    def __init__(self, result):
        self._result = result

    def whenDone(self):
        return succeed(self._result)

    def pause(self):
        raise TaskDone()

    def resume(self):
        pass

    def stop(self):
        raise TaskDone()

class SubscriptionManagerTests(TestCase):
    """
    Tests for the subscription manager's HTTP interface.
    """
    @given(subscription_id(), furl(), bucket_name())
    def test_round_trip(self, subscription_id, introducer_furl, bucket_name):
        """
        A subscription created with a PUT to /v1/subscriptions/<id> can be
        retrieved with a GET of /v1/subscriptions.
        """
        root = make_resource(FilePath(mkdtemp().decode("utf-8")))
        agent = MemoryAgent(root)
        client = SMClient(endpoint=b"", agent=agent)

        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        kwargs = dict(cooperator=Uncooperator())

        d = client.create(subscription_id, details, kwargs)
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

        root = make_resource(FilePath(mkdtemp().decode("utf-8")))
        agent = MemoryAgent(root)
        client = SMClient(endpoint=b"", agent=agent)

        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        kwargs = dict(cooperator=Uncooperator())

        self.successResultOf(client.create(id_a, details, kwargs))
        self.successResultOf(client.create(id_b, details, kwargs))

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
        root = make_resource(FilePath(mkdtemp().decode("utf-8")))
        agent = MemoryAgent(root)
        client = SMClient(endpoint=b"", agent=agent)

        details = dict(
            introducer_pem=u"introducer pem",
            storage_pem=u"storage pem",
            storage_privkey=u"storage privkey",
            bucket_name=bucket_name,
            introducer_furl=introducer_furl,
        )
        kwargs = dict(cooperator=Uncooperator())

        self.successResultOf(client.create(subscription_id, details, kwargs))
        self.successResultOf(client.delete(subscription_id))

        subscriptions = self.successResultOf(client.list())
        self.assertThat(subscriptions, Equals([]))
