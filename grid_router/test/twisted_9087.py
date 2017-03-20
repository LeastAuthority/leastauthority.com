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
            if same_address(port, self._hostAddress.port, self._hostAddress.host):
                ports.remove(port)
                break
        return succeed(None)


def same_address(server, port, interface):
    listen_interface = server[-1] or '0.0.0.0'
    return server[0] == port and listen_interface == interface


_listenTCP = MemoryReactor.listenTCP
def listenTCP(self, port, factory, backlog=50, interface=''):
    for server in self.tcpServers:
        if same_address(server, port, interface):
            raise ValueError(u"Already listening on {}:{}".format(interface, port))
    port = _listenTCP(self, port, factory, backlog, interface)
    return _FakePort(
        reactor=self,
        hostAddress=port._hostAddress,
        kind="tcpServers",
    )


def patch():
    MemoryReactor.listenTCP = listenTCP
