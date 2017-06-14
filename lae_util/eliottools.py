# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Eliot-related functionality.
"""

from __future__ import absolute_import, unicode_literals

from json import loads

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
