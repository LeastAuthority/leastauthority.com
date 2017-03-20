"""
Hotfix for https://twistedmatrix.com/trac/ticket/9087
"""

import attr

from zope.interface import implementer

from twisted.internet.interfaces import IListeningPort
from twisted.internet.defer import succeed
from twisted.test.proto_helpers import MemoryReactor

@implementer(IListeningPort)
@attr.s(frozen=True)
class _FakePort(object):
    _reactor = attr.ib()
    _hostAddress = attr.ib()
    _kind = attr.ib()

    def stopListening(self):
        ports = getattr(self._reactor, self._kind)
        for port in ports:
            interface = port[-1] or '0.0.0.0'
            if port[0] == self._hostAddress.port and interface == self._hostAddress.host:
                ports.remove(port)
                break
        return succeed(None)


_listenTCP = MemoryReactor.listenTCP
def listenTCP(self, *a, **kw):
    port = _listenTCP(self, *a, **kw)
    return _FakePort(
        reactor=self,
        hostAddress=port._hostAddress,
        kind="tcpServers",
    )


def patch():
    MemoryReactor.listenTCP = listenTCP
