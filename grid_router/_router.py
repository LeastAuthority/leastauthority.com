"""
A self-reconfiguring proxy for TCP connections to Kubernetes pods runninig
Tahoe-LAFS introducers and storage server.

The proxy reconfigures itself based on Kubernetes Pods it observes to exist.
"""

import sys

from twisted.python.usage import Options as _Options
from twisted.protocols.portforward import ProxyFactory
from twisted.internet.protocol import Factory, Protocol
from twisted.internet.endpoints import TCP4ServerEndpoint, TCP4ClientEndpoint, serverFromString
from twisted.application.service import MultiService, Service
from twisted.application.internet import StreamServerEndpointService, TimerService

from foolscap.negotiate import Negotiation
from foolscap.tokens import BananaError, NegotiationError
from foolscap.util import isSubstring

from pyrsistent import freeze, pmap, pset

from eliot import (
    Message, start_action, FileDestination, add_destination, remove_destination,
)
from eliot.twisted import DeferredContext

from lae_automation.kubeclient import KubeClient
from lae_automation.subscription_converger import (
    KubernetesClientOptionsMixin, get_customer_grid_pods, divert_errors_to_log,
)



class Options(_Options, KubernetesClientOptionsMixin):
    optParameters = [
        ("interval", None, 10.0, "The interval (in seconds) at which to iterate on reconfiguration.", float),
    ]

    def postOptions(self):
        KubernetesClientOptionsMixin.postOptions(self)



def makeService(options, reactor=None):
    if reactor is None:
        # Boo global reactor
        # https://twistedmatrix.com/trac/ticket/9063
        from twisted.internet import reactor
    return grid_router_service(
        reactor,
        options.get_kubernetes_service(reactor).client(),
        options["kubernetes-namespace"].decode("ascii"),
        options["interval"],
    )



def grid_router_service(reactor, k8s, kubernetes_namespace, interval):
    """
    Create an ``IService`` which can route connections to the correct grid.
    """
    service = _GridRouterParent()

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
    def route_mapping(self):
        return self.getServiceNamed(_GridRouterService.name).route_mapping()


class _EliotLogging(Service):
    def startService(self):
        self._destination = FileDestination(sys.stdout)
        add_destination(self._destination)


    def stopService(self):
        remove_destination(self._destination)



class _FoolscapProxy(Negotiation):
    # Basically just copied from foolscap/negotiate.py so we get the tub id
    # extraction logic but we can then do something different with it.
    def handlePLAINTEXTServer(self, header):
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

        try:
            port_func, pod = self.factory.route_mapping()[targetTubID]
        except KeyError:
            raise NegotiationError("unknown TubID %s" % (targetTubID,))

        ip = pod.status.podID
        if not ip:
            raise NegotiationError("TubID not yet available %s" % (targetTubID,))

        port_number = port_func(pod)

        # Now proxy to ip:port_number
        proxy(self, TCP4ClientEndpoint(self.factory.reactor, ip, port_number))



def proxy(upstream, endpoint):
    def failed(reason):
        upstream.transport.resumeProducing()
        upstream.transport.abortConnection()
        return reason

    upstream.transport.pauseProducing()
    d = endpoint.connect(Factory.forProtocol(_Proxy))
    d.addCallbacks(
        lambda downstream: downstream.take_over(upstream),
        failed,
    )



class _Proxy(Protocol):
    def take_over(self, upstream):
        upstream.dataReceived = self.transport.write
        upstream.connectionLost = self._upstream_connection_lost

        self.dataReceived = upstream.transport.write
        self.upstream = upstream
        self.upstream.transport.resumeProducing()

    def _cleanup(self):
        del self.upstream.dataReceived
        del self.upstream.connectionLost

        del self.dataReceived
        del self.upstream


    def _upstream_connection_lost(self, reason):
        self.transport.abortConnection()
        self._cleanup()


    def connectionLost(self, reason):
        self.upstream.transport.abortConnection()
        self._cleanup()



class _GridRouterService(MultiService):
    """
    ``_GridRouterService`` accepts connections on many ports and proxies them
    to the pod responsible for them.  Responsibility is determined by the
    local port number.
    """
    name = u"grid-router"

    def __init__(self, reactor):
        MultiService.__init__(self)
        # _EliotLogging().setServiceParent(self)
        self._reactor = reactor
        self._route_mapping = freeze({})


    def factory(self):
        f = Factory.forProtocol(_FoolscapProxy)
        f.reactor = self._reactor
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
        self._route_mapping = self._pods_to_routes(self._route_mapping, pods)


    def _pods_to_routes(self, old, pods):
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


    def _address_for_pod(self, pod):
        """
        Determine the address (IP or DNS name) at which a Pod can be reached.
        """
        address = pod.status.podIP
        if not address:
            raise ValueError("no podIP")
        return address


    def _proxy_for_container_port(self, address, container_port):
        """
        Create an ``IService`` which can proxy for a single port.

        :param unicode address: The IP address to which to proxy.

        :param v1.ContainerPort container_port: The port on which and to which
            to proxy.
        """
        return StreamServerEndpointService(
            TCP4ServerEndpoint(self._reactor, container_port.containerPort),
            ProxyFactory(address, container_port.containerPort),
        )



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
