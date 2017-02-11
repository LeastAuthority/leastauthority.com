"""
Tests for ``lae_automation.subscription_converger``.
"""

from hypothesis import assume, given
from hypothesis.strategies import lists, choices, randoms

from pyrsistent import thaw, pmap, pset

from eliot import Message, start_action

from testtools.assertions import assert_that
from testtools.matchers import Equals

from txaws.testing.service import FakeAWSServiceRegion
from txaws.route53.model import HostedZone
from txaws.route53.client import Name, CNAME

from lae_util.testtools import TestCase

from txkube.testing.matchers import PClassEquals, MappingEquals

from lae_automation.subscription_manager import (
    memory_client,
)
from lae_automation.subscription_converger import (
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

from .strategies import subscription_details, deployment_configuration
from ..kubeclient import KubeClient

from txkube import v1, memory_kubernetes

class ConvergeHelperTests(TestCase):
    """
    Tests for ``converge`` helpers.
    """
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


from hypothesis.stateful import RuleBasedStateMachine, rule

from twisted.python.filepath import FilePath

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
        service = new_service()
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
        service = new_service().transform(
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

    def execute_step(self, *a, **kw):
        with self.action.context():
            super(SubscriptionConvergence, self).execute_step(*a, **kw)

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
            not in self.database.list_subscriptions_identifiers()
        )
        Message.log(activating=details.subscription_id)
        self.database.create_subscription(
            subscription_id=details.subscription_id,
            details=details,
        )

    @rule(choose=choices())
    def deactivate(self, choose):
        identifiers = self.database.list_subscriptions_identifiers()
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
            subscriptions = sorted(database.list_subscriptions_identifiers())
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
                Equals(k8s_state.configmaps.item_by_name(configmap_name(sid))),
            )

    def check_deployments(self, database, config, subscriptions, k8s_state, aws):
        for sid in subscriptions:
            assert_that(
                create_deployment(config, database.get_subscription(sid)),
                Equals(k8s_state.deployments.item_by_name(deployment_name(sid))),
            )

    def check_service(self, database, config, subscriptions, k8s_state, aws):
        expected = new_service()
        for sid in subscriptions:
            expected = add_subscription_to_service(
                expected, database.get_subscription(sid),
            )
        assert_that(
            expected,
            PClassEquals(k8s_state.services.item_by_name(expected.metadata.name)),
        )
        Message.log(check_service=thaw(expected))

    def check_route53(self, database, config, subscriptions, k8s_state, aws):
        expected_rrsets = pmap({
            Name(
                u"{subscription_id}.introducer.{domain}".format(
                    subscription_id=sid,
                    domain=config.domain,
                )
            ): pset({
                CNAME(Name(
                    u"introducer.{domain}".format(domain=config.domain)
                ))
            })
            for sid in subscriptions
        })
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
        assert_that(
            expected_rrsets,
            MappingEquals(actual_rrsets),
        )
class SubscriptionConvergenceTests(SubscriptionConvergence.TestCase):
    def test_convergence(self):
        self.runTest()
