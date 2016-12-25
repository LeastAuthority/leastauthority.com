# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
An implementation of ``twisted.internet.task.Cooperator`` which
does not do any cooperation.  This allows to to be independent of the
reactor.  This is often useful for unit tests.
"""

from twisted.python.failure import Failure
from twisted.internet.defer import succeed
from twisted.internet.task import TaskDone

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
