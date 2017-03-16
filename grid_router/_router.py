"""
A self-reconfiguring proxy for TCP connections to Kubernetes pods runninig
Tahoe-LAFS introducers and storage server.

The proxy reconfigures itself based on Kubernetes Pods it observes to exist.
"""

from twisted.python.usage import Options as _Options
from twisted.protocols.portforward import ProxyFactory
from twisted.internet.endpoints import TCP4ServerEndpoint
from twisted.application.service import MultiService
from twisted.application.internet import StreamServerEndpointService, TimerService

from eliot import Message, start_action
from eliot.twisted import DeferredContext

from lae_automation.kubeclient import KubeClient
from lae_automation.subscription_converger import (
    KubernetesClientOptionsMixin, get_customer_grid_pods,
)


class Options(_Options, KubernetesClientOptionsMixin):
    optParameters = [
        ("interval", None, 10.0, "The interval (in seconds) at which to iterate on reconfiguration.", float),
    ]

    def postOptions(self):
        KubernetesClientOptionsMixin.postOptions(self)



def makeService(options):
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
    service = MultiService()
    router = _GridRouterService(reactor)
    router.setServiceParent(service)
    updater = _RouterUpdateService(interval, k8s, kubernetes_namespace, router)
    updater.setServiceParent(service)
    return service



class _GridRouterService(MultiService):
    """
    ``_GridRouterService`` accepts connections on many ports and proxies them
    to the pod responsible for them.  Responsibility is determined by the
    local port number.
    """
    def __init__(self, reactor):
        MultiService.__init__(self)
        self._reactor = reactor


    def set_pods(self, pods):
        """
        Update grid routing rules based on new information about what pods exist.

        :param list[v1.Pod] pods: The pods which were observed to exist very
            recently.
        """
        pod_names = set()

        # Look at what should be routed but maybe isn't currently.
        for pod in pods:
            name = pod.metadata.name
            # Build up a set of names for later.
            pod_names.add(name)

            if name not in self.namedServices:
                # We're not currently servicing this pod.  We should do so.
                try:
                    pod_service = self._service_for_pod(pod)
                except ValueError as e:
                    Message.log(event_type=u"router-update:add", not_ready=unicode(e), pod=name)
                else:
                    Message.log(event_type=u"router-update:add", ready=True, pod=name)
                    pod_service.setServiceParent(self)

        # Look at what is routed currently but maybe shouldn't be.
        for service in self.namedServices.itervalues():
            if service.name not in pod_names:
                # We're currently providing service but there is no longer any
                # corresponding pod.
                Message.log(event_type=u"router-update:remove", pod=service.name)
                service.disownServiceParent()


    def _service_for_pod(self, pod):
        """
        Create an ``IService`` which will perform the necessary proxying for
        ``pod``.

        :param v1.Pod pod:
        """
        address = self._address_for_pod(pod)
        s = MultiService()
        s.name = pod.metadata.name
        for container in pod.spec.containers:
            proxy = self._proxy_for_container_port(address, container.ports[0])
            proxy.setServiceParent(s)
        return s


    def _dns_name_for_pod(self, pod):
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
    def __init__(self, interval, k8s, namespace, router):
        TimerService.__init__(self, interval, self._check_once, k8s, namespace)
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
