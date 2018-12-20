# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``grid_router``.
"""

from socket import AF_INET, socket

from testtools.matchers import (
    AfterPreprocessing,
    Equals,
)

from hypothesis import given, assume
from hypothesis.strategies import (
    data,
    sampled_from,
)
from hypothesis.stateful import RuleBasedStateMachine, rule, run_state_machine_as_test

from twisted.python.log import msg
from twisted.trial.unittest import TestCase as AsyncTestCase
from twisted.internet.address import IPv4Address
from twisted.internet.interfaces import IReactorTCP, IReactorTime
from twisted.internet.defer import Deferred
from twisted.internet.endpoints import AdoptedStreamServerEndpoint, TCP4ServerEndpoint
from twisted.test.proto_helpers import StringTransport, MemoryReactor
from twisted.python.components import proxyForInterface
from twisted.internet.task import Clock, deferLater

from pyrsistent import freeze

from foolscap.pb import Tub
from foolscap.referenceable import Referenceable

from lae_util.testtools import TestCase
from lae_automation.test.strategies import (
    ipv4_addresses, port_numbers, node_pems,
    deployment_configuration, subscription_details,
)
from lae_automation.containers import create_deployment
from lae_automation.model import NullDeploymentConfiguration, SubscriptionDetails

from lae_util.k8s import derive_pod

from .. import Options, makeService
from .._router import _GridRouterService

from txkube import memory_kubernetes, v1_5_model as model



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
        self.model = self.client.model

        self.deploy_config = NullDeploymentConfiguration()
        # Set a few dummy values that we know create_deployment requires.
        self.deploy_config.kubernetes_namespace = u"testing"
        self.deploy_config.introducer_image = u"example-invalid/tahoe-introducer"
        self.deploy_config.storageserver_image = u"example-invalid/tahoe-storageserver"

        self.used_tubs = set()
        self.pods = {}
        # Keep deployments alive so they can provide a unique identifier for
        # pod naming.
        self.deployments = []

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
        self.case.addCleanup(self.service.stopService)


    @rule(
        ip=ipv4_addresses(),
        storage_pem=node_pems(), storage_port=port_numbers(),
        intro_pem=node_pems(), intro_port=port_numbers(),
    )
    def create_pod(self, ip, storage_pem, storage_port, intro_pem, intro_port):
        """
        A new customer grid pod shows up, as would happen if a new user just
        signed up and got provisioned.
        """
        assume(
            storage_pem != intro_pem
            and Tub(storage_pem).getTubID() not in self.used_tubs
            and Tub(intro_pem).getTubID() not in self.used_tubs
        )
        details = SubscriptionDetails(
            bucketname=u"foo",
            # Set the node secrets.  From these, tub identifiers can be
            # derived.
            oldsecrets={
                # Storage server.
                u"server_node_pem": storage_pem,
                u"introducer_node_pem": intro_pem,
            },
            customer_email=u"foo",
            customer_pgpinfo=u"foo",
            product_id=u"foo",
            customer_id=u"foo",
            subscription_id=u"foo",
            stripe_subscription_id=u"foo",
            introducer_port_number=intro_port,
            storage_port_number=storage_port,
        )
        deployment = create_deployment(
            self.deploy_config,
            details,
            self.model,
        )
        self.deployments.append(deployment)
        pod = derive_pod(self.model, deployment, ip)
        self.case.successResultOf(self.client.create(pod))

        self.pods[pod] = (ip, storage_pem, storage_port, intro_pem, intro_port)
        self.used_tubs.update({
            Tub(storage_pem).getTubID(),
            Tub(intro_pem).getTubID(),
        })


    @rule(data=data())
    def remove_pod(self, data):
        """
        An existing customer grid pod goes away, as would happen if a user
        cancelled their subscription.
        """
        assume(0 < len(self.pods))
        pod, values = data.draw(sampled_from(sorted(self.pods.items())))
        _, storage_pem, _, intro_pem, _ = values
        del self.pods[pod]
        self.used_tubs.difference_update({
            Tub(storage_pem).getTubID(),
            Tub(intro_pem).getTubID(),
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

        # GridRouter ought to have a mapping from the active subscription tub
        # identifiers to the internal addresses that own those tubs.
        mapping = self.service.route_mapping()

        expected = {}
        for pod, values in self.pods.iteritems():
            (ip, storage_pem, storage_port, intro_pem, intro_port) = values
            expected[Tub(storage_pem).getTubID()] = (ip, storage_port)
            expected[Tub(intro_pem).getTubID()] = (ip, intro_port)

        self.case.assertThat(
            mapping,
            AfterPreprocessing(
                lambda m: {
                    tub_id: address
                    for (tub_id, (pod, address))
                    in m.iteritems()
                },
                Equals(expected),
            ),
        )



class GridRouterTests(TestCase):
    """
    Test the states and transitions of ``GridRouter``.
    """
    def test_grid_router(self):
        run_state_machine_as_test(lambda: GridRouterStateMachine(self))


    @given(
        ip=ipv4_addresses(),
        deploy_config=deployment_configuration(),
        details=subscription_details()
    )
    def test_pods_to_routes(self, ip, deploy_config, details):
        reactor = object()
        service = _GridRouterService(reactor)
        deployment = create_deployment(deploy_config, details, model)
        pod = derive_pod(model, deployment, ip)
        service.set_pods([pod])
        mapping = service.route_mapping()
        self.assertThat(
            mapping,
            AfterPreprocessing(
                lambda m: {
                    tub_id: address
                    for (tub_id, (pod, address))
                    in m.iteritems()
                },
                Equals({
                    details.introducer_tub_id: (ip, details.introducer_port_number),
                    details.storage_tub_id: (ip, details.storage_port_number),
                }),
            ),
        )

    @given(
        ip=ipv4_addresses(),
        deploy_config=deployment_configuration(),
        details=subscription_details()
    )
    def test_proxy(self, ip, deploy_config, details):
        network = MemoryReactor()
        clock = Clock()
        reactor = FakeReactor(network, clock)
        service = _GridRouterService(reactor)
        deployment = create_deployment(deploy_config, details, model)
        pod = derive_pod(model, deployment, ip)
        service.set_pods([pod])
        factory = service.factory()
        protocol = factory.buildProtocol(None)

        transport = StringTransport()
        protocol.makeConnection(transport)
        protocol.dataReceived((
            u"GET /id/{} HTTP/1.1\r\n"
            u"Host: example.invalid\r\n"
            u"\r\n"
        ).format(details.introducer_tub_id).encode("ascii"))

        self.assertThat(
            network.connectors.pop(0).getDestination(),
            Equals(IPv4Address("TCP", ip, details.introducer_port_number)),
        )



# XXX Doesn't seem to be easily reversible.
from foolscap.logging.log import bridgeLogsToTwisted
bridgeLogsToTwisted(lambda event: True)


class FoolscapProxyTests(AsyncTestCase):
    """
    Tests for ``_FoolscapProxy``.
    """
    def setUp(self):
        from twisted.internet import reactor
        self.reactor = reactor

    def tearDown(self):
        # XXX Lots of Foolscap selectables left in the reactor without this.
        return deferLater(self.reactor, 0.1, lambda: None)

    # XXX This hangs sometimes while waiting for connectTo to succeed.
    def test_proxying(self):
        pems = node_pems()

        # Create client and server tubs we can use to speak Foolscap through
        # the proxy.  This should demonstrate the proxy is working.
        client = Tub(pems.example())
        client.startService()
        self.addCleanup(client.stopService)

        server = Tub(pems.example())
        server.startService()
        self.addCleanup(server.stopService)

        # Get an arbitrary local address on which the server tub can listen.
        while True:
            server_socket = socket()
            try:
                server_socket.bind(("", 0))
            except Exception as e:
                print(e)
                server_socket.close()
            else:
                server_socket.listen(1)
                self.addCleanup(server_socket.close)
                break

        server_address = server_socket.getsockname()
        msg("Got server address {}".format(server_address))
        server_endpoint = AdoptedStreamServerEndpoint(
            self.reactor, server_socket.fileno(), AF_INET,
        )
        server.listenOn(server_endpoint)

        # Get a grid router that knows where the server tub really is and so
        # should be able to proxy connections to it for us.
        grid_router = _GridRouterService(self.reactor)
        grid_router.set_route_mapping(freeze({
            server.getTubID().decode("ascii"): (None, server_address),
        }))

        # Start the proxy listening.
        factory = grid_router.factory()
        d = TCP4ServerEndpoint(self.reactor, 0).listen(factory)

        def listening(proxy_port):
            self.addCleanup(proxy_port.stopListening)

            # Tell the server tub where the _proxy_ is it generates a furl
            # pointing at the proxy.  We'll use that furl to connect with the
            # client tub and rely on the proxy to get us to the right place.
            host = proxy_port.getHost().host.decode("ascii")
            port_number = proxy_port.getHost().port
            location = u"{host}:{port}".format(
                host=host,
                port=port_number,
            )
            server.setLocation(location.encode("ascii"))
            msg("Set server Tub location {}".format(location))
            # Register something arbitrary that we can poke at.
            furl = server.registerReference(Referenceable())

            # Try to connect to the server tub with the client tub through the
            # proxy.
            d = Deferred()
            reconn = client.connectTo(furl, d.callback)
            self.addCleanup(reconn.stopConnecting)
            return d

        d.addCallback(listening)
        def connected(ref):
            pass
        d.addCallback(connected)
        return d

    # xasdasd
