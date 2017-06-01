
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
        result of the asynchronousservice factory function it is constructed
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
