# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
A self-reconfiguring proxy for TCP connections to Kubernetes pods runninig
Tahoe-LAFS introducers and storage server.

The proxy reconfigures itself based on Kubernetes Pods it observes to exist.

This module is used with ``twisted.application.service.ServiceMaker`` in
``lae_dropin.py``.  ``ServiceMaker`` uses ``Options`` and ``makeService`` to
expose this code via ``twist`` and ``twistd``.
"""

from twisted.python.log import msg
# Rename this so we can have a module attribute named Options.  Stick with the
# attribute-import style (as opposed to just importing ``usage``) to get early
# warning of mistakes.
from twisted.python.usage import Options as _Options
from twisted.internet.defer import Deferred
from twisted.internet.protocol import Factory, Protocol
from twisted.internet.endpoints import TCP4ClientEndpoint, serverFromString
from twisted.application.service import MultiService
from twisted.application.internet import StreamServerEndpointService, TimerService

from foolscap.tokens import BananaError, NegotiationError
from foolscap.util import isSubstring

from pyrsistent import freeze, pmap, pset

from eliot import (
    Message,
    start_action,
)
from eliot.twisted import DeferredContext

from lae_util.service import AsynchronousService
from lae_util.fluentd_destination import (
    eliot_logging_service,
    opt_eliot_destination,
)
from lae_automation.kubeclient import KubeClient
from lae_automation.subscription_converger import (
    KubernetesClientOptionsMixin, get_customer_grid_pods, divert_errors_to_log,
)



class Options(_Options, KubernetesClientOptionsMixin):
    """
    Command-line option definitions for *twist[d] s4-grid-router*.
    """
    optParameters = [
        ("interval", None, 10.0,
         "The interval (in seconds) at which to iterate on reconfiguration.", float,
        ),
    ]

    opt_eliot_destination = opt_eliot_destination

    def postOptions(self):
        KubernetesClientOptionsMixin.postOptions(self)



def makeService(options, reactor=None):
    """
    Make a grid router service.

    :param Options options: Choices about configuration for the service.

    :param reactor: A Twisted reactor to give to the service for its eventing
        needs.

    :return IService: A service which is capable of accepting Foolscap
        connections and routing/proxying them to the appropriate destination.
    """
    if reactor is None:
        # Boo global reactor
        # https://twistedmatrix.com/trac/ticket/9063
        from twisted.internet import reactor

    parent = _GridRouterParent()

    eliot_logging_service(
        reactor,
        options.get("destinations", []),
    ).setServiceParent(parent)

    def make_service():
        kubernetes = options.get_kubernetes_service(reactor)
        d = kubernetes.versioned_client()
        d.addCallback(
            lambda client: grid_router_service(
                reactor,
                client,
                options["kubernetes-namespace"].decode("ascii"),
                options["interval"],
            )
        )
        return d

    service = AsynchronousService(make_service)
    service.setServiceParent(parent)

    return parent



def grid_router_service(reactor, k8s, kubernetes_namespace, interval):
    """
    Create an ``IService`` which can route connections to the correct grid.
    """
    service = _GridRouterParent()
    service.setName(_GridRouterService.name)

    router = _GridRouterService(reactor)
    router.setServiceParent(service)

    updater = _RouterUpdateService(reactor, interval, k8s, kubernetes_namespace, router)
    updater.setServiceParent(service)

    StreamServerEndpointService(
        serverFromString(reactor, "tcp:10000"),
        router.factory(),
    ).setServiceParent(service)

    return service



class _GridRouterParent(MultiService):
    """
    A service container for the services that make up the grid router.
    """
    def route_mapping(self):
        """
        Proxy through to ``_GridRouterService.route_mapping``.

        This is provided so users don't have to reach into this service's
        children (creating the freedom to re-arrange those children should we
        later wish to).

        :see: ``_GridRouterService.route_mapping``
        """
        return self.getServiceNamed(_GridRouterService.name).route_mapping()



class _FoolscapProxy(Protocol):
    """
    A protocol which speaks just enough of the first part of a Foolscap
    conversation to extract the TubID so that a proxy target can be selected
    based on that value.

    :ivar bytes buffered: Data which has been received and buffered but not
        yet interpreted or passed on.
    """
    buffered = b""

    def dataReceived(self, data):
        """
        Buffer the received data until enough is received that we can determine a
        proxy destination.
        """
        msg("_FoolscapProxy.dataReceived {!r}".format(data))
        self.buffered += data
        if b"\r\n\r\n" in self.buffered:
            header = self.buffered
            self.buffered = b""
            self.handlePLAINTEXTServer(header)

    # Basically just copied from foolscap/negotiate.py so we get the tub id
    # extraction logic but we can then do something different with it.
    def handlePLAINTEXTServer(self, header):
        """
        Parse a complete HTTP-like Foolscap negotiation request and begin proxying
        to a destination selected based on the extract TubID.
        """
        # the client sends us a GET message
        lines = header.split("\r\n")
        if not lines[0].startswith("GET "):
            raise BananaError("not right")
        command, url, version = lines[0].split()
        if not url.startswith("/id/"):
            # probably a web browser
            raise BananaError("not right")
        targetTubID = url[4:]

        Message.log(event_type=u"handlePLAINTEXTServer", tub_id=targetTubID)

        if targetTubID == "":
            # they're asking for an old UnauthenticatedTub. Refuse.
            raise NegotiationError("secure Tubs require encryption")
        if isSubstring("Upgrade: TLS/1.0\r\n", header):
            wantEncrypted = True
        else:
            wantEncrypted = False

        Message.log(event_type=u"handlePLAINTEXTServer", want_encrypted=wantEncrypted)

        self._handleTubRequest(header, targetTubID)


    def _handleTubRequest(self, header, targetTubID):
        """
        Proxy to the destination which is responsible for the target TubID.

        :param bytes header: The first part of the Foolscap negotiation which
            will need to be passed along to the proxy target.

        :param bytes targetTubID: The TubID which was requested.
        """
        try:
            _, (ip, port_number) = self.factory.route_mapping()[targetTubID]
        except KeyError:
            raise NegotiationError("unknown TubID %s" % (targetTubID,))

        if not ip:
            raise NegotiationError("TubID not yet available %s" % (targetTubID,))

        # Now proxy to ip:port_number
        proxy(self, TCP4ClientEndpoint(self.factory.reactor, ip, port_number), header)



def proxy(upstream, endpoint, header):
    """
    Establish a new connection to ``endpoint`` and begin proxying between that
    connection and ``upstream``.

    :param IProtocol upstream: A connected protocol.  All data received by
        this protocol from this point on will be sent along to another newly
        established connection.

    :param IStreamClientEndpoint endpoint: An endpoint to use to establish a
        new connection.  All data received over this connection will be sent
        along to the upstream connection.

    :param bytes header: Some extra data to write to the new downstream
        connection before proxying begins.
    """
    def failed(reason):
        upstream.transport.resumeProducing()
        upstream.transport.abortConnection()
        return reason

    upstream.transport.pauseProducing()

    peer = upstream.transport.getPeer()
    action = start_action(
        action_type=u"grid-router:proxy",
        **{u"from": (peer.host, peer.port)}
    )
    with action.context():
        d = DeferredContext(endpoint.connect(Factory.forProtocol(_Proxy)))
        d.addCallbacks(
            lambda downstream: DeferredContext(downstream.take_over(upstream, header)),
            failed,
        )
        return d.addActionFinish()



class _Proxy(Protocol):
    """
    Handle the downstream connection for a proxy between two connections.
    """
    def take_over(self, upstream, header):
        """
        Begin actively proxying between this protocol and ``upstream``.

        :param Protocol: The upstream connection involved in this proxying
            operation.  It will be abused somewhat.  Read the implementation.

        :param bytes header: Any data that should be sent downstream before
            engaging the proxy.

        :return Deferred: A ``Deferred`` that fires when this protocol's
            connection is lost.  This should be tightly coupled to loss of the
            upstream protocol's connection.
        """
        self.done = Deferred()

        peer = self.transport.getPeer()
        a = start_action(
            action_type=u"grid-router:proxy:take-over",
            to=(peer.host, peer.port),
        )
        with a:
            self.transport.write(header)

            upstream.dataReceived = self.transport.write
            upstream.connectionLost = self._upstream_connection_lost

            self.dataReceived = upstream.transport.write
            self.upstream = upstream
            self.upstream.transport.resumeProducing()
            return self.done


    def _upstream_connection_lost(self, reason):
        """
        The upstream connection was lost.  Close this connection as well.
        """
        self.transport.abortConnection()


    def connectionLost(self, reason):
        """
        This protocol's connection was lost.  Close the upstream connection as
        well.
        """
        self.upstream.transport.abortConnection()
        del self.upstream.dataReceived
        del self.upstream.connectionLost

        del self.dataReceived
        self.upstream = None
        self.done.callback(None)



class _GridRouterService(MultiService):
    """
    ``_GridRouterService`` accepts connections on many ports and proxies them
    to the pod responsible for them.  Responsibility is determined by the
    local port number.

    :ivar _reactor: A Twisted reactor which can be used to establish
        connections for the proxy.

    :ivar _route_mapping: A mapping from tub identifiers to destination
        information.  The destination information is a two-tuple of an IP
        address and a port number.  It gives an address where a Foolscap node
        capable of servicing the tub identifier can be reached.
    """
    name = u"grid-router"

    def __init__(self, reactor):
        MultiService.__init__(self)
        self._reactor = reactor
        self._route_mapping = freeze({})


    def factory(self):
        """
        :return: A protocol factory for a Foolscap proxy server.
        """
        f = Factory.forProtocol(_FoolscapProxy)
        f.reactor = self._reactor
        f.route_mapping = self.route_mapping
        return f


    def route_mapping(self):
        """
        Retrieve the mapping describing how to route connections to pods.

        :return PMap: A mapping from a tub identifier to a (host, port) pair.
        """
        return self._route_mapping


    def set_pods(self, pods):
        """
        Update grid routing rules based on new information about what pods exist.

        :param list[v1.Pod] pods: The pods which were observed to exist very
            recently.
        """
        self.set_route_mapping(self._pods_to_routes(self._route_mapping, pods))


    def set_route_mapping(self, route_mapping):
        """
        Record a new route mapping.

        :param route_mapping: A new value for the private ``_route_mapping``
            attribute.
        """
        self._route_mapping = freeze(route_mapping)


    def _pods_to_routes(self, old, pods):
        """
        Extract the addressing information from some pods.

        :param old: The old routing information.  Used to log route changes.

        :param list[v1.Pod] pods: Some pods from which routing information can
            be extracted.

        :return: A mapping of the new routing information deriving solely from
            ``pods``.
        """
        def _introducer_tub(pod):
            return pod.metadata.annotations[u"leastauthority.com/introducer-tub-id"]
        def _storage_tub(pod):
            return pod.metadata.annotations[u"leastauthority.com/storage-tub-id"]

        def _introducer_port_number(pod):
            return int(pod.metadata.annotations[u"leastauthority.com/introducer-port-number"])
        def _storage_port_number(pod):
            return int(pod.metadata.annotations[u"leastauthority.com/storage-port-number"])

        def _introducer_address(pod):
            return (pod.status.podIP, _introducer_port_number(pod))
        def _storage_address(pod):
            return (pod.status.podIP, _storage_port_number(pod))

        with start_action(action_type=u"router-update:set-pods", count=len(pods)):
            new = pmap([
                (_introducer_tub(pod), (pod, _introducer_address(pod)))
                for pod in pods
            ] + [
                (_storage_tub(pod), (pod, _storage_address(pod)))
                for pod in pods
            ])

            adding = pset(new.keys()) - pset(old.keys())
            removing = pset(old.keys()) - pset(new.keys())

            for tub_id in adding:
                Message.log(event_type=u"router-update:add", pod=new[tub_id][0].metadata.name)
            for tub_id in removing:
                Message.log(event_type=u"router-update:remove", pod=old[tub_id][0].metadata.name)

            return new



class _RouterUpdateService(TimerService):
    """
    ``_RouterUpdateService`` reports valid Pods to a ``_GridRouterService``.
    """
    def __init__(self, reactor, interval, k8s, namespace, router):
        TimerService.__init__(
            self,
            interval,
            divert_errors_to_log(self._check_once, u"router-update"),
            k8s,
            namespace,
        )
        # This attribute controls the the reactor used by TimerService to set
        # up the LoopingCall.
        self.clock = reactor
        self._router = router


    def _check_once(self, k8s, namespace):
        """
        Load the customer grid pods from Kubernetes.
        """
        a = start_action(action_type=u"router-update:check")
        with a.context():
            d = DeferredContext(
                get_customer_grid_pods(KubeClient(k8s=k8s), namespace)
            )
            d.addCallback(self._router.set_pods)
            return d.addActionFinish()
