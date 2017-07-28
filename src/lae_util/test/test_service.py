
from testtools.matchers import Equals

from twisted.internet.defer import Deferred
from twisted.application.service import Service, MultiService

from ..service import AsynchronousService

from ..testtools import TestCase


class AsynchronousServiceTests(TestCase):
    """
    Tests for ``AsynchronousServiceTests``.
    """
    def test_replacement(self):
        """
        An ``AsynchronousService`` instance is replaced in its parent with the
        result of the asynchronous service factory function it is constructed
        with.
        """
        d = Deferred()
        def factory():
            return d
        p = MultiService()
        a = AsynchronousService(factory)
        a.setServiceParent(p)

        a.startService()
        r = Service()
        d.callback(r)

        self.expectThat(list(p), Equals([r]))


    def test_stop(self):
        """
        If an ``AsynchronousService`` is stopped before the factory function's
        ``Deferred`` fires, the ``Deferred`` is cancelled.
        """
        cancelled = []
        d = Deferred(canceller=cancelled.append)
        def factory():
            return d
        p = MultiService()
        a = AsynchronousService(factory)
        a.setServiceParent(p)

        a.startService()
        a.stopService()
        self.assertEqual([d], cancelled)
        self.assertEqual(False, a.running)
