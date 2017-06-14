# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_util.eliottools``.
"""

from __future__ import print_function, unicode_literals

from testtools.matchers import Equals, IsInstance, ContainsDict

from twisted.logger import Logger as TwistedLogger

from eliot import MemoryLogger as EliotLogger
from eliot.testing import capture_logging

from ..testtools import TestCase
from ..eliottools import TwistedLoggerToEliotObserver


class TwistedLoggerToEliotObserverTests(TestCase):
    """
    Tests for ``TwistedLoggerToEliotObserver``.
    """
    def test_relaying(self):
        """
        Log events emitted via a ``twisted.logger.Logger`` with a
        ``TwistedLoggerToEliotObserver`` end up published to the
        ``eliot.Logger`` the observer was constructed with.
        """
        eliot_logger = EliotLogger()
        self._relaying_test(
            eliot_logger,
            TwistedLoggerToEliotObserver(eliot_logger),
        )


    @capture_logging(None)
    def test_default_logger(self, eliot_logger):
        """
        ``TwistedLoggerToEliotObserver`` created without a logger publishes to the
        default logger.
        """
        self._relaying_test(
            eliot_logger,
            TwistedLoggerToEliotObserver(),
        )


    def _relaying_test(self, eliot_logger, observer):
        """
        Publish an event using ``twisted.logger`` with ``observer`` hooked up and
        assert that the event ends up being seen by ``eliot_logger``.
        """
        twisted_logger = TwistedLogger(observer=observer)
        twisted_logger.info("Hello, world.")

        [event] = eliot_logger.messages

        self.assertThat(
            event,
            ContainsDict(dict(
                # A couple things from the Twisted side of the fence.
                log_namespace=Equals("lae_util.test.test_eliot"),
                log_format=Equals("Hello, world."),
                # And also some Eliot stuff.
                task_uuid=IsInstance(unicode),
                task_level=IsInstance(list),
            )),
        )
