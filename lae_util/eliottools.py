# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Eliot-related functionality.
"""

from __future__ import absolute_import, unicode_literals

from json import loads
from logging import (
    INFO,
    Handler,
)
import attr
from attr.validators import optional, provides

from zope.interface import implementer

from eliot import ILogger, Message

from twisted.logger import ILogObserver, eventAsJSON


@implementer(ILogObserver)
@attr.s(frozen=True)
class TwistedLoggerToEliotObserver(object):
    """
    An ``ILogObserver`` which re-publishes events as Eliot messages.
    """
    logger = attr.ib(default=None, validator=optional(provides(ILogger)))

    def _observe(self, event):
        flattened = loads(eventAsJSON(event))
        # We get a timestamp from Eliot.
        flattened.pop("log_time")
        # This is never serializable anyway.
        flattened.pop("log_logger")

        Message.new(**flattened).write(self.logger)


    # The actual ILogObserver interface uses this.
    __call__ = _observe



class _StdlibLoggingToEliotHandler(Handler):
    def __init__(self, logger=None):
        Handler.__init__(self)
        self.logger = logger


    def emit(self, record):
        Message.new(**vars(record)).write(self.logger)



def stdlib_logging_to_eliot_configuration(stdlib_logger, eliot_logger=None):
    """
    Add a handler to ``stdlib_logger`` which will relay events to
    ``eliot_logger`` (or the default Eliot logger if ``eliot_logger`` is
    ``None``).
    """
    handler = _StdlibLoggingToEliotHandler(eliot_logger)
    handler.set_name("eliot")
    handler.setLevel(INFO)
    stdlib_logger.addHandler(handler)
    return lambda: stdlib_logger.removeHandler(handler)
