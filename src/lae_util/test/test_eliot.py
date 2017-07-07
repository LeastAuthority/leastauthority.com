# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_util.eliottools``.
"""

from __future__ import print_function, unicode_literals

import logging

from testtools.matchers import Equals, IsInstance, ContainsDict

from twisted.python.reflect import fullyQualifiedName
from twisted.logger import Logger as TwistedLogger

from eliot import MemoryLogger as EliotLogger
from eliot.testing import capture_logging

from ..testtools import TestCase
from ..eliottools import (
    TwistedLoggerToEliotObserver,
    stdlib_logging_to_eliot_configuration,
)


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



class StdlibLoggingToELiotHandlerTests(TestCase):
    """
    Tests for ``_StdlibLoggingToEliotHandler``.
    """
    def test_relaying(self):
        """
        When ``stdlib_logging_to_eliot_configuration`` is used on a
        ``logging.Logger``, in force, log events emitted via that logger are
        published to the ``eliot.Logger`` the handler was constructed with.
        """
        eliot_logger = EliotLogger()
        self._relaying_test(eliot_logger, eliot_logger)


    @capture_logging(None)
    def test_default_logger(self, eliot_logger):
        """
        ``stdlib_logging_to_eliot_configuration`` uses the default Eliot logger
        when it is not passed one.
        """
        self._relaying_test(None, eliot_logger)


    def _relaying_test(self, eliot_logger_publish, eliot_logger_consume):
        """
        Publish an event using ``logger.Logger` with an Eliot relay handler hooked
        up to the root logger and assert that the event ends up b eing seen by
        ``eliot_logger_consumer`.
        """
        cleanup = stdlib_logging_to_eliot_configuration(
            logging.getLogger(),
            eliot_logger_publish,
        )
        self.addCleanup(cleanup)

        logger = logging.getLogger(fullyQualifiedName(self.__class__))
        logger.setLevel(logging.INFO)
        logger.info("Hello, world.")

        [event] = eliot_logger_consume.messages
        self.assertThat(
            event,
            ContainsDict(dict(
                # A couple things from the stdlib side of the fence.
                module=Equals(__name__.split(".")[-1]),
                levelno=Equals(logging.INFO),
                # Also some Eliot stuff.
                task_uuid=IsInstance(unicode),
                task_level=IsInstance(list),
            )),
        )
