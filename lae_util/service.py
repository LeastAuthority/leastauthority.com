import attr

from zope.interface import implementer

from twisted.application.service import IService, IServiceCollection

from eliot import start_action, write_failure
from eliot.twisted import DeferredContext


@implementer(IService)
@attr.s
class AsynchronousService(object):
    """
    An ``IService`` which replaces itself with the result of an asynchronous
    operation when that operation completes.
    """
    _factory = attr.ib()
    _d = attr.ib(init=False, default=None)

    _parent = attr.ib(init=False, default=None)
    running = attr.ib(init=False, default=False)
    name = attr.ib(init=False, default=None)

    def setName(self, name):
        self.name = name


    def setServiceParent(self, parent):
        if self._parent is not None:
            self.disownServiceParent()
        parent = IServiceCollection(parent, parent)
        self._parent = parent
        self._parent.addService(self)


    def disownServiceParent(self):
        d = self._parent.removeService(self)
        self._parent = None
        return d


    def privilegedStartService(self):
        pass


    def startService(self):
        with start_action(action_type=u"asyncservice:start"):
            self.running = True
            d = self._d = DeferredContext(self._factory())
            d.addCallback(self._created)
            d.addErrback(self._failed)


    def stopService(self):
        self.running = False
        if self._d is not None:
            d = self._d
            self._d = None
            d.cancel()


    def _created(self, service):
        self._d = None
        service.setServiceParent(self._parent)
        self.disownServiceParent()


    def _failed(self, reason):
        self._d = None
        write_failure(reason)
