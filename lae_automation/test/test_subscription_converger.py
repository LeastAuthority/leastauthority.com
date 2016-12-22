"""
Tests for ``lae_automation.subscription_converger``.
"""

from functools import partial

import attr
from hypothesis import assume, given

from testtools.assertions import assert_that
from testtools.matchers import Equals

from lae_util.testtools import TestCase

from .matchers import MappingEquals

from lae_automation.subscription_manager import (
    memory_client,
)
from lae_automation.subscription_converger import (
    converge,
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


from hypothesis.stateful import RuleBasedStateMachine, rule

from twisted.python.filepath import FilePath

from tempfile import mkdtemp

from lae_automation.subscription_manager import SubscriptionDatabase


class SubscriptionConvergence(RuleBasedStateMachine):
    def __init__(self):
        super(SubscriptionConvergence, self).__init__()
        self.path = FilePath(mkdtemp().decode("utf-8"))
        self.database = SubscriptionDatabase.from_directory(self.path)
        
    @rule(details=subscription_details())
    def activate(self, details):
        assume(
            details.subscription_id
            not in self.database.list_subscriptions_identifiers()
        )
        self.database.create_subscription(
            subscription_id=details.subscription_id,
            details=details,
        )

    @rule(config=deployment_configuration())
    def converge(self, config):
        client = memory_client(self.database.path)
        kube = MemoryKube()
        aws = MemoryAWS()
        d = converge(config, client, kube, aws)
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        assert d.called
        assert d.result is None, d.result.getTraceback()
        self.check_convergence(self.database, config, kube, aws)

    def check_convergence(self, database, config, kube, aws):
        subscriptions = database.list_subscriptions_identifiers()
        checks = {
            self.check_configmaps,
            self.check_deployments,
            self.check_service,
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

    def check_route53(self, database, config, subscriptions, kube, aws):
        self.fail("write this")


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
        xs = getattr(self, definition["kind"].lower())
        xs[definition["metadata"]["name"]] = definition

    apply = create


class MemoryAWS(object):
    creds = object()

    def get_client(self, cls, **kw):
        return cls(**kw)


class SubscriptionConvergenceTests(SubscriptionConvergence.TestCase):
    def test_convergence(self):
        self.runTest()
