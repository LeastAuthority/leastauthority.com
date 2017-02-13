"""
Tests for ``lae_automation.subscription_converger``.
"""

from json import dumps

from zope.interface.verify import verifyObject

from hypothesis import assume, given
from hypothesis.strategies import lists, choices, randoms
from hypothesis import Verbosity, settings

from pyrsistent import thaw, pmap

from eliot import Message, start_action

from testtools.assertions import assert_that
from testtools.matchers import (
    AfterPreprocessing, Equals, Is, Not, MatchesPredicate, LessThan,
    GreaterThan, MatchesAll, MatchesRegex,
)

from twisted.python.filepath import FilePath
from twisted.application.service import IService

from txaws.testing.service import FakeAWSServiceRegion
from txaws.route53.model import RRSetKey, RRSet, HostedZone
from txaws.route53.client import Name, CNAME

from lae_util.testtools import TestCase

from lae_automation.test.matchers import GoodEquals

from lae_automation.subscription_manager import (
    memory_client,
)
from lae_automation.subscription_converger import (
    _introducer_name_for_subscription,
    Options, makeService,
    get_customer_grid_service,
    converge, get_hosted_zone_by_name, apply_service_changes,
)
from lae_automation.containers import (
    service_ports,
    new_service,
    introducer_port_name,
    storage_port_name,
    create_configuration,
    create_deployment,
    configmap_name,
    deployment_name,
    add_subscription_to_service,
)

from .strategies import subscription_id, subscription_details, deployment_configuration
from ..kubeclient import KubeClient

from txkube import v1, memory_kubernetes

CERTIFICATE = FilePath(__file__).parent().child("cert.pem")
KEY = CERTIFICATE.sibling("key.pem")


def is_lower():
    return MatchesPredicate(
        lambda text: text.lower() == text,
        u"%s is not lowercase",
    )


def longer_than(n):
    return AfterPreprocessing(len, GreaterThan(n))

def shorter_than(n):
    return AfterPreprocessing(len, LessThan(n))


class ConvergeHelperTests(TestCase):
    """
    Tests for ``converge`` helpers.
    """
    @given(subscription_id())
    def test_introducer_name_for_subscription(self, sid):
        """
        ``_introducer_name_for_subscription`` returns a legal DNS name for the
        given subscription identifier.
        """
        domain = _introducer_name_for_subscription(sid, u"example.com")
        self.expectThat(
            domain,
            AfterPreprocessing(
                unicode,
                MatchesAll(
                    is_lower(),
                    longer_than(0),
                    shorter_than(256),
                    MatchesRegex(ur"^[a-z0-9.-]+$"),
                    AfterPreprocessing(
                        lambda domain: domain.split(u"."),
                        MatchesAll(
                            longer_than(0),
                            shorter_than(64),
                        ),
                    ),
                ),
            ),
        )

    @given(subscription_details())
    def test_service_ports(self, details):
        """
        ``service_ports`` returns a two-element list containing mappings
        which expose the subscription's introducer and storage ports.
        """
        self.assertThat(
            service_ports(details),
            Equals([
                v1.ServicePort(
                    name=introducer_port_name(details.subscription_id),
                    port=details.introducer_port_number,
                    targetPort=introducer_port_name(details.subscription_id),
                    protocol=u"TCP",
                ),
                v1.ServicePort(
                    name=storage_port_name(details.subscription_id),
                    port=details.storage_port_number,
                    targetPort=storage_port_name(details.subscription_id),
                    protocol=u"TCP",
                ),
            ]),
        )

    def test_get_hosted_zone_by_name_missing(self):
        region = FakeAWSServiceRegion(
            access_key="access key id",
            secret_key="secret access key",
        )
        route53 = region.get_route53_client()
        d = get_hosted_zone_by_name(route53, u"example.invalid")
        self.failureResultOf(d, KeyError)

    def test_get_hosted_zone_by_name(self):
        zone = HostedZone(
            name=u"example.invalid",
            identifier=u"",
            rrset_count=0,
            reference=u"unique string",
        )
        region = FakeAWSServiceRegion(
            access_key="access key id",
            secret_key="secret access key",
        )
        route53 = region.get_route53_client()
        d = route53.create_hosted_zone(zone.reference, zone.name)
        self.successResultOf(d)
        d = get_hosted_zone_by_name(route53, zone.name)
        retrieved = self.successResultOf(d)
        self.expectThat(zone.reference, Equals(retrieved.reference))
        self.expectThat(zone.name, Equals(retrieved.name))


    def test_customer_grid_service(self):
        """
        The ``v1.Service`` for the customer grid can be retrieved using
        ``get_customer_grid_service``.
        """
        kubernetes = memory_kubernetes()
        client = KubeClient(k8s=kubernetes.client())

        # If it doesn't exist, we should get ``None``.
        service = self.successResultOf(get_customer_grid_service(client, u"default"))
        self.expectThat(service, Is(None))

        # If it does exist, we should get it!
        self.successResultOf(client.create(new_service(u"default")))
        service = self.successResultOf(get_customer_grid_service(client, u"default"))
        # A weak assertion working around
        # https://github.com/LeastAuthority/txkube/issues/94
        self.expectThat(service, Not(Is(None)))



class MakeServiceTests(TestCase):
    def test_interface(self):
        """
        ``makeService`` returns an object that provides ``IService``.
        """
        config = FilePath(self.mktemp())
        config.setContent(dumps({
            u"apiVersion": u"v1",
            u"clusters": [{
                u"name": u"testing",
                u"cluster": {
                    u"certificate-authority": CERTIFICATE.path,
                    u"server": u"https://bar/",
                },
            }],
            u"users": [{
                u"name": u"testing",
                u"user": {
                    u"client-certificate": CERTIFICATE.path,
                    u"client-key": KEY.path,
                },
            }],
            u"contexts": [{
                u"name": u"testing",
                u"context": {
                    u"cluster": u"testing",
                    u"user": u"testing",
                    u"namespace": u"testing",
                },
            }],
        }))
        options = Options()
        options.parseOptions([
            b"--endpoint", b"http://localhost:8000/",
            b"--k8s-context", u"testing",
            b"--k8s-config", config.path,
        ])
        verifyObject(IService, makeService(options))


from hypothesis.stateful import RuleBasedStateMachine, rule, run_state_machine_as_test

from tempfile import mkdtemp

from lae_automation.subscription_manager import SubscriptionDatabase


class ApplyServiceChangesTests(TestCase):
    """
    Tests for ``apply_service_changes``.
    """
    @given(lists(subscription_details(), average_size=1))
    def test_create(self, details):
        """
        ``apply_service_changes`` adds entries based on the ``to_create``
        subscriptions to the service's ``ports``.
        """
        service = new_service(u"testing")
        changed = apply_service_changes(service, to_delete=set(), to_create=set(details))
        self.assertThat(
            set(changed.spec.ports),
            Equals(
                set(p for d in details for p in service_ports(d)),
            ),
        )

    @given(lists(subscription_details(), min_size=1, average_size=3), randoms())
    def test_delete(self, details, random):
        """
        ``apply_service_changes`` removes entries based on the ``to_delete``
        subscriptions from the service's ``ports``.
        """
        service = new_service(u"testing").transform(
            [u"spec", u"ports"],
            list(p for d in details for p in service_ports(d)),
        )
        to_delete = set(
            d.subscription_id
            for d
            in details
            if random.choice((True, False))
        )
        changed = apply_service_changes(service, to_delete=to_delete, to_create=set())
        self.assertThat(
            set(changed.spec.ports),
            Equals(
                set(
                    p
                    for d in details if d.subscription_id not in to_delete
                    for p in service_ports(d)),
            ),
        )



class SubscriptionConvergence(RuleBasedStateMachine):
    def __init__(self):
        super(SubscriptionConvergence, self).__init__()
        self.path = FilePath(mkdtemp().decode("utf-8"))
        self.database = SubscriptionDatabase.from_directory(self.path)

        self.deploy_config = deployment_configuration().example()

        self.subscription_client = memory_client(self.database.path)
        self.kubernetes = memory_kubernetes()
        self.kube_client = KubeClient(k8s=self.kubernetes.client())
        self.aws_region = FakeAWSServiceRegion(
            access_key="access_key_id",
            secret_key="secret_access_key",
        )
        route53 = self.aws_region.get_route53_client()
        d = route53.create_hosted_zone(
            caller_reference=u"opaque reference",
            name=self.deploy_config.domain,
        )
        # XXXX
        assert d.called, d
        self.zone = d.result
        self.action = start_action(action_type=u"convergence-test")

    def execute_step(self, step):
        with self.action.context():
            rule = step[0]
            function = rule.function
            with start_action(action_type=u"step", rule=function.__name__):
                super(SubscriptionConvergence, self).execute_step(step)

    def teardown(self):
        self.action.finish()

    @rule(details=subscription_details())
    def activate(self, details):
        """
        Activate a new subscription in the subscription manager.

        This makes new subscription state available to subsequently
        executed rules.
        """
        assume(
            details.subscription_id
            not in self.database.list_all_subscription_identifiers()
        )
        Message.log(activating=details.subscription_id)
        self.database.create_subscription(
            subscription_id=details.subscription_id,
            details=details,
        )

    @rule(choose=choices())
    def deactivate(self, choose):
        identifiers = self.database.list_active_subscription_identifiers()
        assume(0 < len(identifiers))
        subscription_id = choose(sorted(identifiers))
        Message.log(deactivating=subscription_id)
        self.database.deactivate_subscription(subscription_id)

    @rule()
    def converge(self):
        """
        Converge the cluster (Kubernetes, Route53, etc) configuration on
        the state in the subscription manager.

        This uses subscription state from previous ``activate`` and
        ``deactivate`` rules as well as state in the cluster resulting
        from previous convergence attempts.
        """
        d = converge(
            self.deploy_config,
            self.subscription_client,
            self.kube_client,
            self.aws_region,
        )
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        assert d.called
        assert d.result is None, d.result.getTraceback()
        self.check_convergence(
            self.database,
            self.deploy_config,
            self.kube_client,
            self.aws_region,
        )

    def check_convergence(self, database, config, kube, aws):
        with start_action(action_type=u"check-convergence"):
            subscriptions = sorted(database.list_active_subscription_identifiers())
            Message.log(active_subscriptions=subscriptions)
            checks = {
                self.check_configmaps,
                self.check_deployments,
                self.check_service,
                self.check_route53,
            }
            k8s_state = self.kubernetes._state
            for check in checks:
                check(database, config, subscriptions, k8s_state, aws)

    def check_configmaps(self, database, config, subscriptions, k8s_state, aws):
        for sid in subscriptions:
            assert_that(
                create_configuration(config, database.get_subscription(sid)),
                GoodEquals(k8s_state.configmaps.item_by_name(configmap_name(sid))),
            )

    def check_deployments(self, database, config, subscriptions, k8s_state, aws):
        for sid in subscriptions:
            actual = k8s_state.deployments.item_by_name(deployment_name(sid))
            reference = create_deployment(config, database.get_subscription(sid))
            def drop_transients(deployment):
                simplified = deployment.transform(
                    [u"metadata", u"annotations"], {},
                    [u"metadata", u"resourceVersion"], None,
                    [u"status"], None,
                )
                return simplified
            assert_that(
                actual,
                AfterPreprocessing(drop_transients, GoodEquals(reference)),
            )

    def check_service(self, database, config, subscriptions, k8s_state, aws):
        expected = new_service(config.kubernetes_namespace)
        for sid in subscriptions:
            expected = add_subscription_to_service(
                expected, database.get_subscription(sid),
            )
        assert_that(
            expected,
            GoodEquals(k8s_state.services.item_by_name(expected.metadata.name)),
        )
        Message.log(check_service=thaw(expected))

    def check_route53(self, database, config, subscriptions, k8s_state, aws):
        expected_rrsets = pmap()
        for sid in subscriptions:
            label = _introducer_name_for_subscription(sid, config.domain)
            key = RRSetKey(label=label, type=u"CNAME")
            cname = CNAME(Name(u"introducer.{domain}".format(domain=config.domain)))
            rrset = RRSet(label=label, type=u"CNAME", ttl=60, records={cname})
            expected_rrsets = expected_rrsets.set(key, rrset)

        route53 = aws.get_route53_client()
        d = route53.list_resource_record_sets(self.zone.identifier)
        # XXX
        result = d.result

        actual_rrsets = pmap({
            key: rrset
            for (key, rrset)
            in result.iteritems()
            # Don't care about these infrastructure rrsets.
            if key.type not in (u"SOA", u"NS")
        })
        if actual_rrsets != expected_rrsets:
            import pdb; pdb.set_trace()
        assert_that(
            actual_rrsets,
            GoodEquals(expected_rrsets),
        )

class SubscriptionConvergenceTests(TestCase):
    def test_convergence(self):
        run_state_machine_as_test(
            self._machine,
            settings(verbosity=Verbosity.verbose),
        )

    def _machine(self):
        # Clear the Eliot logs at the beginning of every run
        # Maybe just the last run's logs are good enough?
        self.clear_logs()
        return SubscriptionConvergence()
