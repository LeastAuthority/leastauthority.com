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

from testtools.matchers import AfterPreprocessing, Equals, Is

from hypothesis import given

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
        self.expectThat(subscription, Equals(dict(version=1, details=expected)))
