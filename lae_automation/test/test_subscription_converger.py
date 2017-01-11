"""
Tests for ``lae_automation.subscription_converger``.
"""

from functools import partial

import attr
from hypothesis import assume, given
from pyrsistent import thaw, pmap, pset

from json import loads, dumps

from eliot import Message

from testtools.assertions import assert_that
from testtools.matchers import Equals

from txaws.testing.service import FakeAWSServiceRegion
from txaws.route53.model import HostedZone
from txaws.route53.client import Name, CNAME

from lae_util.testtools import TestCase

from .matchers import MappingEquals

from lae_automation.subscription_manager import (
    memory_client,
)
from lae_automation.subscription_converger import (
    converge, get_hosted_zone_by_name,
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
                {
                    "name": introducer_port_name(details.subscription_id),
                    "port": details.introducer_port_number,
                    "targetPort": introducer_port_name(details.subscription_id),
                    "protocol": "TCP",
                },
                {
                    "name": storage_port_name(details.subscription_id),
                    "port": details.storage_port_number,
                    "targetPort": storage_port_name(details.subscription_id),
                    "protocol": "TCP",
                },
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


class SubscriptionConvergence(RuleBasedStateMachine):
    def __init__(self):
        super(SubscriptionConvergence, self).__init__()
        self.path = FilePath(mkdtemp().decode("utf-8"))
        self.database = SubscriptionDatabase.from_directory(self.path)

        self.deploy_config = deployment_configuration().example()

        self.subscription_client = memory_client(self.database.path)
        self.kube_client = MemoryKube()
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
        self.database.create_subscription(
            subscription_id=details.subscription_id,
            details=details,
        )

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
        subscriptions = database.list_subscriptions_identifiers()
        checks = {
            self.check_configmaps,
            self.check_deployments,
            self.check_service,
            self.check_route53,
        }
        for check in checks:
            check(database, config, subscriptions, kube, aws)

    def check_configmaps(self, database, config, subscriptions, kube, aws):
        for sid in subscriptions:
            assert_that(
                create_configuration(config, database.get_subscription(sid)),
                Equals(kube.get_configmaps(name=configmap_name(sid))),
            )

    def check_deployments(self, database, config, subscriptions, kube, aws):
        for sid in subscriptions:
            assert_that(
                create_deployment(config, database.get_subscription(sid)),
                Equals(kube.get_deployments(name=deployment_name(sid))),
            )

    def check_service(self, database, config, subscriptions, kube, aws):
        expected = new_service()
        for sid in subscriptions:
            expected = add_subscription_to_service(
                expected, database.get_subscription(sid),
            )
        assert_that(
            expected,
            MappingEquals(kube.get_services(name=expected["metadata"]["name"])),
        )
        Message.log(check_service=thaw(expected))

    def check_route53(self, database, config, subscriptions, kube, aws):
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
        actual_rrsets = d.result
        assert_that(
            expected_rrsets,
            MappingEquals(actual_rrsets),
        )


def selectors_match(selectors, resource):
    missing = object()
    return {
        key: resource["metadata"]["labels"].get(key, missing)
        for key in selectors
    } == selectors

@attr.s(frozen=True)
class MemoryKube(object):
    resources = attr.ib(default=attr.Factory(dict))

    def _resource(key):
        return property(lambda self: self.resources.setdefault(key, {}))

    configmap = _resource("configmap")
    deployment = _resource("deployment")
    service = _resource("service")

    def _get(self, xs, name=None, selectors=None):
        if name is not None:
            return xs[name]
        if selectors is not None:
            return filter(partial(selectors_match, selectors), xs.itervalues())
        return xs.itervalues()

    def get_configmaps(self, **kwargs):
        return self._get(self.configmap, **kwargs)

    def get_deployments(self, **kwargs):
        return self._get(self.deployment, **kwargs)

    def get_services(self, **kwargs):
        return self._get(self.service, **kwargs)

    def destroy(self, kind, name):
        xs = getattr(self, kind.lower())
        del xs[name]

    def create(self, definition):
        Message.log(memorykube_create=definition)
        xs = getattr(self, definition["kind"].lower())
        # Round-trip through JSON encoding to prove it is JSON encodable.
        xs[definition["metadata"]["name"]] = loads(dumps(definition))

    apply = create


class SubscriptionConvergenceTests(SubscriptionConvergence.TestCase):
    def test_convergence(self):
        self.runTest()
