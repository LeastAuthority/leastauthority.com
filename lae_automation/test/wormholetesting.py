"""
An in-memory implementation of some of the magic-wormhole interfaces for
use by automated tests.
"""

from itertools import count

from zope.interface import Interface, implementer

from twisted.internet.defer import DeferredQueue, Deferred, succeed

try:
    from wormhole._interfaces import IWormhole
except ImportError:
    # 0.9.2 compatibility
    IWormhole = Interface


class MemoryWormholeServer(object):
    """
    A factory for in-memory wormholes.
    """
    def __init__(self):
        self._apps = {}


    def create(self, appid, relay_url, reactor, tor=None):
        """
        Create a wormhole.  It will be able to connect to other wormholes created
        by this ``MemoryWormholeServer`` instance (and constrained by the
        normal appid/relay_url rules).
        """
        if tor is not None:
            raise ValueError("Cannot deal with Tor right now.")

        key = (relay_url, appid)
        return _MemoryWormhole(self._view(key))


    def _view(self, key):
        return _WormholeServerView(self, key)


    # 0.9.2 compatibility
    __call__ = create



class _WormholeApp(object):
    """
    Represent a collection of wormholes that belong to the same
    appid/relay_url scope.
    """
    def __init__(self):
        self.wormholes = {}
        self._waiting = {}
        self._counter = count()


    def allocate_code(self, wormhole, code):
        """
        Allocate a new code for the given wormhole.

        This also associates the given wormhole with the code for future
        lookup.

        Code generation logic is trivial and certainly not good enough for any
        real use.  It is sufficient for automated testing, though.
        """
        if code is None:
            code = u"{}-persnickety-tardigrade".format(next(self._counter))
        self.wormholes.setdefault(code, []).append(wormhole)
        try:
            waiters = self._waiting.pop(code)
        except KeyError:
            pass
        else:
            for w in waiters:
                w.callback(wormhole)

        return code


    def wait_for_wormhole(self, code):
        """
        Return a ``Deferred`` which fires with the next wormhole to be associated
        with the given code.  This is used to let the first end of a wormhole
        rendezvous with the second end.
        """
        d = Deferred()
        self._waiting.setdefault(code, []).append(d)
        return d



class _WormholeServerView(object):
    """
    Present an interface onto the server to be consumed by individual
    wormholes.
    """
    def __init__(self, server, key):
        self._server = server
        self._key = key


    def allocate_code(self, wormhole, code):
        """
        Allocate a new code for the given wormhole in the scope associated with
        this view.
        """
        app = self._server._apps.setdefault(self._key, _WormholeApp())
        return app.allocate_code(wormhole, code)


    def wormhole_by_code(self, code, exclude):
        """
        Retrieve all wormholes previously associated with a code.
        """
        app = self._server._apps[self._key]
        wormholes = app.wormholes[code]
        try:
            [wormhole] = list(
                wormhole
                for wormhole
                in wormholes
                if wormhole != exclude
            )
        except ValueError:
            return app.wait_for_wormhole(code)
        return succeed(wormhole)



@implementer(IWormhole)
class _MemoryWormhole(object):
    """
    Represent one side of a wormhole as conceived by ``MemoryWormholeServer``.
    """
    _code = None

    def __init__(self, view):
        self._view = view
        self._payload = DeferredQueue()


    def allocate_code(self):
        self._code = self._view.allocate_code(self, None)


    def set_code(self, code):
        if self._code is None:
            self._code = code
            self._view.allocate_code(self, code)
        else:
            raise ValueError(
                "set_code used with a wormhole which already has a code"
            )


    def when_code(self):
        if self._code is None:
            raise NotImplementedError(
                "This implementation requires allocate_code before when_code."
            )
        return succeed(self._code)
    get_code = when_code


    def get_welcome(self):
        return succeed('welcome')


    def send(self, payload):
        self._payload.put(payload)


    def when_received(self):
        if self._code is None:
            raise ValueError(
                "This implementation requires set_code or allocate_code "
                "before when_received."
            )
        d = self._view.wormhole_by_code(self._code, exclude=self)

        def got_wormhole(wormhole):
            return wormhole._payload.get()
        d.addCallback(got_wormhole)
        return d
    get_message = when_received


    def close(self):
        pass


    # 0.9.2 compatibility
    def get_code(self):
        self.allocate_code()
        return self.when_code()

    get = when_received
