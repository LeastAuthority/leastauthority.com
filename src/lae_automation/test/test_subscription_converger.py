# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_automation.subscription_converger``.
"""

from json import dumps

from zope.interface.verify import verifyObject

import pem
import attr

from hypothesis import assume, given, settings
from hypothesis.strategies import choices, data

from pyrsistent import thaw, pmap, discard

from eliot import Message, start_action
from eliot.testing import capture_logging

from testtools.assertions import assert_that
from testtools.matchers import (
    AfterPreprocessing, Equals, Is, Not, MatchesPredicate, LessThan,
    GreaterThan, MatchesAll, MatchesRegex, Contains, HasLength,
    MatchesAny,
)

from twisted.python.filepath import FilePath
from twisted.application.service import IService
from twisted.python.failure import Failure

from txaws.testing.service import FakeAWSServiceRegion
from txaws.route53.model import RRSetKey, RRSet, HostedZone
from txaws.route53.client import Name, CNAME

from lae_util.k8s import (
    derive_pod, derive_replicaset, get_replicasets, get_pods,
)
from lae_util.testtools import TestCase, CustomException

from lae_automation.test.matchers import GoodEquals

from lae_automation.subscription_manager import (
    memory_client,
)
from lae_automation.subscription_converger import (
    _introducer_name_for_subscription,
    Options, makeService,
    get_customer_grid_service,
    converge, get_hosted_zone_by_name,
    divert_errors_to_log,
)
from lae_automation.containers import (
    S4_CUSTOMER_GRID_NAME,
    new_service,
    create_configuration,
    create_deployment,
    configmap_name,
    deployment_name,
)
from lae_automation.signup import get_bucket_name

from ..stripe import (
    StripeSubscription,
    StripeState,
    memory_stripe_client,
)
from .strategies import (
    domains, subscription_id, subscription_details, deployment_configuration,
    node_pems, ipv4_addresses, docker_image_tags,
)
from ..kubeclient import KubeClient

from txkube import memory_kubernetes

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
        d = get_hosted_zone_by_name(route53, Name(zone.name))
        retrieved = self.successResultOf(d)
        self.expectThat(retrieved.zone.reference, Equals(zone.reference))
        self.expectThat(retrieved.zone.name, Equals(zone.name))


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
        self.successResultOf(client.create(new_service(u"default", client.k8s.model)))
        service = self.successResultOf(get_customer_grid_service(client, u"default"))
        # A weak assertion working around
        # https://github.com/LeastAuthority/txkube/issues/94
        self.expectThat(service, Not(Is(None)))



class MakeServiceTests(TestCase):
    def test_interface(self):
        """
        ``makeService`` returns an object that provides ``IService``.
        """
        scratch = FilePath(self.mktemp())
        scratch.makedirs()

        cert_and_key = node_pems().example()
        cert, key = pem.parse(cert_and_key)
        scratch.child(u"cert.pem").setContent(cert.as_bytes())
        scratch.child(u"key.pem").setContent(key.as_bytes())

        config = scratch.child(u"config.json")
        config.setContent(dumps({
            u"apiVersion": u"v1",
            u"clusters": [{
                u"name": u"testing",
                u"cluster": {
                    u"certificate-authority": scratch.child(u"cert.pem").path,
                    u"server": u"https://bar/",
                },
            }],
            u"users": [{
                u"name": u"testing",
                u"user": {
                    u"client-certificate": scratch.child(u"cert.pem").path,
                    u"client-key": scratch.child(u"key.pem").path,
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
        access_key_id_path = FilePath(self.mktemp())
        access_key_id_path.setContent(b"foo")
        secret_access_key_path = FilePath(self.mktemp())
        secret_access_key_path.setContent(b"bar")
        options = Options()
        options.parseOptions([
            b"--domain", b"s4.example.com",
            b"--kubernetes-namespace", b"testing",
            b"--endpoint", b"http://localhost:8000/",
            b"--aws-access-key-id-path", access_key_id_path.path,
            b"--aws-secret-access-key-path", secret_access_key_path.path,
            b"--introducer-image", b"introducer",
            b"--storageserver-image", b"storageserver",
            b"--kubernetes", b"kubernetes",
            b"--k8s-context", u"testing",
            b"--k8s-config", config.path,
        ])
        service = makeService(options)
        verifyObject(IService, service)


from hypothesis.stateful import RuleBasedStateMachine, rule, run_state_machine_as_test

from tempfile import mkdtemp

from lae_automation.subscription_manager import SubscriptionDatabase


class SubscriptionConvergence(RuleBasedStateMachine):
    def __init__(self, case):
        super(SubscriptionConvergence, self).__init__()

        case.clear_logs()

        self.case = case
        self.path = FilePath(mkdtemp().decode("utf-8"))
        self.domain = u"s4.example.com"
        self.database = SubscriptionDatabase.from_directory(
            self.path, self.domain,
        )
        self.stripe_state = StripeState()
        self.stripe_client = memory_stripe_client(self.stripe_state)

        # Track which subscriptions have had these resources created in
        # Kubernetes (by Kubernetes).  Once they've been created, we expect
        # them to continue to exist until the subscription is deactivated.
        self.has_replicaset = set()
        self.has_pod = set()

        self.deploy_config = deployment_configuration().example()

        self.subscription_client = memory_client(self.database.path, self.domain)
        self.kubernetes = memory_kubernetes()
        self.kube_model = self.kubernetes.model
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
        self.zone = self.case.successResultOf(d)
        self.action = start_action(action_type=u"convergence-test")

    def execute_step(self, step):
        with self.action.context():
            rule = step[0]
            function = rule.function
            with start_action(action_type=u"step", rule=function.__name__):
                super(SubscriptionConvergence, self).execute_step(step)

    def teardown(self):
        self.action.finish()


    @rule(details=subscription_details().map(lambda d: attr.assoc(d, oldsecrets=None)))
    def activate(self, details):
        """
        Activate a new subscription in the subscription manager.

        This makes new subscription state available to subsequently
        executed rules.
        """
        sid = details.subscription_id
        assume(
            sid not in self.database.list_all_subscription_identifiers()
        )
        Message.log(activating=sid)
        self.database.create_subscription(
            subscription_id=sid,
            details=details,
        )
        self.stripe_state.subscriptions[sid] = StripeSubscription(
            id=sid,
            status=u"active",
        )


    @rule(choose=choices())
    def cancel_subscription(self, choose):
        """
        Terminate a subscription in the payment processor, Stripe.  This is like
        someone cancelling their subscription or failing to pay.
        """
        identifiers = list(
            sid
            for (sid, subscr)
            in self.stripe_state.subscriptions.items()
            if subscr.status == u"active"
        )
        assume(0 < len(identifiers))

        subscription_id = choose(sorted(identifiers))
        Message.log(deactivating=subscription_id)

        subscr = self.stripe_state.subscriptions[subscription_id]
        self.stripe_state.subscriptions[subscription_id] = attr.assoc(
            subscr,
            status=u"canceled",
        )
        # We no longer require that the pods and replicasets belonging to this
        # subscription exist since the system is supposed to destroy them if
        # there is no corresponding active subscription.  We use ``discard``
        # because we may be deactivating the subscription before we ever got
        # around to creating a Deployment for it (or before the ReplicaSet or
        # Pod for the Deployment got created by the system).
        self.has_replicaset.discard(subscription_id)
        self.has_pod.discard(subscription_id)


    @rule(tag=docker_image_tags())
    def change_tahoe_images(self, tag):
        """
        Change the Deployment configuration to require a different Docker image
        for the Tahoe-LAFS containers.  This essentially corresponds to a
        Tahoe-LAFS upgrade for all customers.
        """
        self.deploy_config = attr.assoc(
            self.deploy_config,
            introducer_image=u"tahoe-introducer:{}".format(tag),
            storageserver_image=u"tahoe-storageserver:{}".format(tag),
        )

    @rule(data=data())
    def allocate_loadbalancer(self, data):
        """
        Complete the S4 Customer Grid service setup by updating its status to
        reflect the existence of a platform-supplied LoadBalancer.  This would
        happen due to actions taken by Kubernetes for any ``LoadBalancer``
        service.
        """
        services = [
            service
            for service
            in self.kubernetes._state.services.items
            if service.spec.type == u"LoadBalancer"
            and service.status is None
        ]
        assume([] != services)
        for service in services:
            self.kubernetes._state_changed(self.kubernetes._state.replace(
                u"services",
                service,
                service.set(
                    u"status",
                    self.kube_model.v1.ServiceStatus(
                        loadBalancer=self.kube_model.v1.LoadBalancerStatus(
                            ingress=[
                                self.kube_model.v1.LoadBalancerIngress(
                                    hostname=data.draw(domains()),
                                ),
                            ],
                        ),
                    ),
                ),
            ))


    @rule()
    def create_replicasets(self):
        """
        Fabricate ReplicaSets which warrant existence and which Kubernetes would
        have made for us had we actually been using it.  This happens
        automatically as a consequence of creating an appropriate Deployment.
        """
        deployments = list(
            deployment
            for deployment
            in self.kubernetes._state.deployments.items
            if 0 == len(get_replicasets(self.kubernetes._state, deployment))
        )
        assume([] != deployments)
        for deployment in deployments:
            self.kubernetes._state_changed(
                self.kubernetes._state.create(
                    u"replicasets",
                    derive_replicaset(self.kube_model, deployment),
                ),
            )
            self.has_replicaset.add(deployment.metadata.annotations[u"subscription"])


    @rule(data=data())
    def create_pods(self, data):
        """
        Fabricate Pods which warrant existence and which Kubernetes would have
        made for us had we actually been using it.  This happens automatically
        as a consequence of creating an appropriate Deployments (by way of
        ReplicaSets).
        """
        deployments = list(
            deployment
            for deployment
            in self.kubernetes._state.deployments.items
            if 0 == len(get_pods(self.kubernetes._state, deployment))
        )
        assume([] != deployments)
        addresses = ipv4_addresses()
        for deployment in deployments:
            self.kubernetes._state_changed(
                self.kubernetes._state.create(
                    u"pods",
                    derive_pod(self.kube_model, deployment, data.draw(addresses)),
                ),
            )
            self.has_pod.add(deployment.metadata.annotations[u"subscription"])


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
            self.stripe_client,
        )
        self.case.successResultOf(d)
        self.check_convergence(
            self.database,
            self.deploy_config,
            self.kube_client,
            self.aws_region,
            self.stripe_state,
        )

    def check_convergence(self, database, config, kube, aws, stripe_state):
        with start_action(action_type=u"check-convergence"):
            subscriptions = sorted(database.list_active_subscription_identifiers())
            Message.log(active_subscriptions=subscriptions)
            checks = {
                self.check_subscriptions,
                self.check_configmaps,
                self.check_deployments,
                self.check_replicasets,
                self.check_pods,
                self.check_service,
                self.check_route53,
                self.check_s3,
            }
            k8s_state = self.kubernetes._state
            for check in checks:
                check(stripe_state, database, config, subscriptions, k8s_state, aws)


    def check_subscriptions(
        self, stripe_state, database, config, subscriptions, kube, aws,
    ):
        """
        If a subscription is cancelled in Stripe, it should have been marked as
        deactivated in the subscription database.
        """
        for sid, subscr in stripe_state.subscriptions.items():
            if subscr.status == u"canceled":
                assert_that(subscriptions, Not(Contains(sid)))
            else:
                assert_that(subscriptions, Contains(sid))


    def check_s3(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        s3 = aws.get_s3_client()
        buckets = set(
            bucket.name
            for bucket
            in self.case.successResultOf(s3.list_buckets())
        )
        for sid in subscriptions:
            subscription = database.get_subscription(sid)
            assert_that(
                buckets,
                Contains(get_bucket_name(sid, subscription.customer_id)),
            )
            # Note we don't check that S3 buckets for deactivated
            # subscriptions don't exist because we're not actually ready to
            # have S3 buckets get deleted automatically yet.


    def check_configmaps(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        for sid in subscriptions:
            assert_that(
                create_configuration(
                    config,
                    database.get_subscription(sid),
                    self.kube_model,
                ),
                GoodEquals(k8s_state.configmaps.item_by_name(configmap_name(sid))),
            )


    def check_pods(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        """
        Any Pods which exist should have an active subscription.  Not all active
        subscriptions necessarily have a Pod.  Any Pods which were created as
        a result of Deployments should also still exist.
        """
        seen = set()
        for pod in k8s_state.pods.items:
            sid = pod.metadata.annotations[u"subscription"]
            seen.add(sid)
            self.case.assertThat(subscriptions, Contains(sid))

        # Every activate subscription for which a pod has been created should
        # still have a pod.  The ``seen`` set we built up in that loop should
        # contain at least the subscriptions in this intersection:
        required = set(subscriptions) & self.has_pod
        self.case.assertThat(
            required,
            MatchesAny(
                LessThan(seen),
                Equals(seen),
            ),
        )


    def check_replicasets(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        """
        Any ReplicaSet which exists should have an active subscription.  Not all
        active subscriptions necessarily have a ReplicaSet.  Any ReplicaSets
        which were created as a result of Deployments should also still exist.
        """
        seen = set()
        for replicaset in k8s_state.replicasets.items:
            sid = replicaset.metadata.annotations[u"subscription"]
            seen.add(sid)
            self.case.assertThat(subscriptions, Contains(sid))

        # Every activate subscription for which a replicaset has been created
        # should still have a replicaset.  The ``seen`` set we built up in
        # that loop should contain at least the subscriptions in this
        # intersection:
        required = set(subscriptions) & self.has_replicaset
        self.case.assertThat(
            required,
            MatchesAny(
                LessThan(seen),
                Equals(seen),
            ),
        )


    def check_deployments(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        for sid in subscriptions:
            actual = k8s_state.deployments.item_by_name(deployment_name(sid))
            reference = create_deployment(
                config,
                database.get_subscription(sid),
                self.kube_model,
            )
            def drop_transients(deployment):
                simplified = deployment.transform(
                    [u"metadata", u"annotations", u"deployment.kubernetes.io/revision"], discard,
                    [u"metadata", u"resourceVersion"], None,
                    [u"status"], None,
                )
                return simplified.serialize()
            assert_that(
                actual,
                AfterPreprocessing(drop_transients, GoodEquals(reference.serialize())),
            )

    def check_service(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        expected = new_service(config.kubernetes_namespace, self.kube_model)
        actual = k8s_state.services.item_by_name(expected.metadata.name)
        # Don't actually care about the status.  That belongs to the server
        # anyway.
        tweaked = actual.set(u"status", None)
        assert_that(
            tweaked,
            GoodEquals(expected),
        )
        Message.log(check_service=thaw(expected))

    def check_route53(
        self, stripe_state, database, config, subscriptions, k8s_state, aws,
    ):
        expected_rrsets = pmap()

        service = k8s_state.services.item_by_name(S4_CUSTOMER_GRID_NAME)
        if service.status is not None:
            # TODO: It would be slightly nicer to make this a Route53 Alias
            # instead of a CNAME.  txAWS needs support for creating Route53 Alias
            # rrsets first, though.
            introducer = RRSetKey(
                label=Name(u"introducer.{domain}".format(domain=config.domain)),
                type=u"CNAME",
            )
            service_ingress = RRSet(
                label=introducer.label,
                type=introducer.type,
                ttl=60,
                records={
                    CNAME(
                        canonical_name=Name(
                            service.status.loadBalancer.ingress[0].hostname,
                        ),
                    ),
                },
            )
            expected_rrsets = expected_rrsets.set(introducer, service_ingress)

        for sid in subscriptions:
            label = _introducer_name_for_subscription(sid, config.domain)
            key = RRSetKey(label=label, type=u"CNAME")
            cname = CNAME(Name(u"introducer.{domain}".format(domain=config.domain)))
            rrset = RRSet(label=label, type=u"CNAME", ttl=60, records={cname})
            expected_rrsets = expected_rrsets.set(key, rrset)

        route53 = aws.get_route53_client()
        d = route53.list_resource_record_sets(self.zone.identifier)
        result = self.case.successResultOf(d)

        actual_rrsets = pmap({
            key: rrset
            for (key, rrset)
            in result.iteritems()
            # Don't care about these infrastructure rrsets.
            if key.type not in (u"SOA", u"NS")
        })
        assert_that(
            actual_rrsets,
            Equals(expected_rrsets),
        )



class SubscriptionConvergenceTests(TestCase):
    def test_convergence(self):
        run_state_machine_as_test(
            self._machine,
        )

    def _machine(self):
        # XXX Should probably clear captured Eliot logs here?  Otherwise we
        # get a mix of logs from all the different cases Hypothesis explores.
        return SubscriptionConvergence(self)


    @given(data())
    @settings(max_shrinks=0)
    def test_service_creation(self, data):
        """
        After the Service is created and its LoadBalancer is allocated, the
        "infrastructure" Route53 state is created (eg the introducer domain
        name).

        This is a regression test derived from ``test_convergence``.  It
        exercises a case where the Service object was not being discovered
        properly because the version had been changed in one place and not
        another.  This led the infrastructure convergence logic to never see
        the LoadBalancer and so never try to create the Route53 state.  It
        also led to many failed attempts to re-create the Service.
        """
        m = self._machine()
        m.converge()
        m.allocate_loadbalancer(data)
        m.converge()


    @given(
        a=subscription_details().map(lambda d: attr.assoc(d, oldsecrets=None)),
        b=subscription_details().map(lambda d: attr.assoc(d, oldsecrets=None)),
        data=data(),
    )
    def test_foo(self, a, b, data):
        m = self._machine()
        m.activate(a)
        m.converge()
        m.create_replicasets()
        m.activate(b)
        m.cancel_subscription(lambda identifiers: a.subscription_id)
        m.create_pods(data)
        m.converge()


class DivertErrorsToLogTests(TestCase):
    """
    Tests for ``divert_errors_to_log``.
    """
    @capture_logging(None)
    def test_failure_logged(self, logger):
        """
        If the decorated function returns a ``Failure``, it is logged.
        """
        def broke():
            return Failure(CustomException())
        divert_errors_to_log(broke, u"test-failure-logged")()
        self.assertThat(logger.flush_tracebacks(CustomException), HasLength(1))
