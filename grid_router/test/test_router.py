"""
Tests for ``grid_router``.
"""

from testtools.matchers import Equals, AfterPreprocessing

from hypothesis import assume
from hypothesis.strategies import choices
from hypothesis.stateful import RuleBasedStateMachine, rule, run_state_machine_as_test

from twisted.internet.interfaces import IReactorTCP, IReactorTime
from twisted.test.proto_helpers import MemoryReactor
from twisted.python.components import proxyForInterface
from twisted.internet.task import Clock

from lae_util.testtools import TestCase
from lae_automation.test.strategies import port_numbers, ipv4_addresses
from lae_automation.containers import create_deployment
from lae_automation.model import NullDeploymentConfiguration, SubscriptionDetails

from .. import Options, makeService

from txkube import v1, memory_kubernetes


class FakeReactor(
        proxyForInterface(IReactorTCP, "_tcp"),
        proxyForInterface(IReactorTime, "_time")
):
    def __init__(self, _tcp, _time):
        self._tcp = _tcp
        self._time = _time



class GridRouterStateMachine(RuleBasedStateMachine):
    def __init__(self, case):
        super(GridRouterStateMachine, self).__init__()

        self.case = case

        self.network = MemoryReactor()
        self.clock = Clock()
        self.reactor = FakeReactor(self.network, self.clock)
        self.kubernetes = memory_kubernetes()
        self.client = self.kubernetes.client()

        self.deploy_config = NullDeploymentConfiguration()
        # Set a few dummy values that we know create_deployment requires.
        self.deploy_config.kubernetes_namespace = u"testing"
        self.deploy_config.introducer_image = u"example-invalid/tahoe-introducer"
        self.deploy_config.storageserver_image = u"example-invalid/tahoe-storageserver"

        self.used_ports = set()
        self.pods = set()

        self.interval = 1.0
        options = Options()
        self.case.patch(
            options,
            "get_kubernetes_service",
            lambda reactor: self.kubernetes,
        )

        options.parseOptions([
            b"--interval", u"{}".format(self.interval).encode("ascii"),
            b"--kubernetes-namespace", self.deploy_config.kubernetes_namespace.encode("ascii"),
            b"--k8s-service-account",
            b"--kubernetes", b"http://127.0.0.1:1234/",
        ])
        self.service = makeService(options, self.reactor)


    @rule()
    def start(self):
        """
        The ``GridRouter`` service starts up.
        """
        assume(not self.service.running)
        self.service.privilegedStartService()
        self.service.startService()


    @rule(port_number=port_numbers(), host_ip=ipv4_addresses())
    def create_pod(self, port_number, host_ip):
        """
        A new customer grid pod shows up, as would happen if a new user just
        signed up and got provisioned.
        """
        assume(
            port_number not in self.used_ports
            and port_number + 1 not in self.used_ports
        )
        self.used_ports.add(port_number)
        self.used_ports.add(port_number + 1)
        details = SubscriptionDetails(
            bucketname=u"foo",
            oldsecrets={},
            customer_email=u"foo",
            customer_pgpinfo=u"foo",
            product_id=u"foo",
            customer_id=u"foo",
            subscription_id=u"foo",
            introducer_port_number=port_number,
            storage_port_number=port_number + 1,
        )
        deployment = create_deployment(self.deploy_config, details)
        # This is roughly how a Deployment creates a Pod... I suppose.
        pod = v1.Pod(
            metadata=dict(
                namespace=deployment.metadata.namespace,
                name=u"{}-{}".format(deployment.metadata.name, port_number),
                labels=deployment.metadata.labels,
            ),
            spec=deployment.spec.template.spec,
            # This is a cheat.  We can't really set the status.  But the
            # in-memory Kubernetes won't set it either so we'd better do it.
            status=v1.PodStatus(
                podIP=host_ip,
            ),
        )
        self.case.successResultOf(self.client.create(pod))
        self.pods.add(pod)


    @rule(choose=choices())
    def remove_pod(self, choose):
        """
        An existing customer grid pod goes away, as would happen if a user
        cancelled their subscription.
        """
        assume(0 < len(self.pods))
        pod = choose(sorted(self.pods))
        self.pods.remove(pod)
        self.used_ports.difference_update({
            port.containerPort
            for container in pod.spec.containers
            for port in container.ports
        })
        self.case.successResultOf(self.client.delete(pod))


    @rule()
    def check(self):
        """
        Examine the routing configuration of the ``GridRouter`` and fail if it
        diverges from what's expected given the pods which currently exist.
        """
        assume(self.service.running)

        # Advance the clock to make sure the router has had a chance to look
        # at the current state.
        self.clock.advance(self.interval)

        # GridRouter ought to have directed the reactor to listen on exactly
        # the ports we've tracked in used_ports.
        self.case.assertThat(
            self.network.tcpServers,
            AfterPreprocessing(
                lambda servers: {server[0] for server in servers},
                Equals(self.used_ports),
            ),
        )



class GridRouterTests(TestCase):
    """
    Test the states and transitions of ``GridRouter``.
    """
    def test_grid_router(self):
        run_state_machine_as_test(lambda: GridRouterStateMachine(self))
