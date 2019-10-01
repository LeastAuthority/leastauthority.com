# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module implements a convergence service which keeps
Kubernetes configuration in line with customer subscriptions.

The service operates in a never-ending loop.  Each iteration of the
loop checks the Kubernetes configuration against the active customer
subscriptions.  If Kubernetes configuration is found which relates to
inactive subscriptions, it is removed.  If subscriptions are found
with no corresponding Kubernetes configuration, such is added for
them.
"""

from os import environ
from functools import partial
from hashlib import sha256

import attr
from pyrsistent import PClass, field

from eliot import Message, start_action, write_failure
from eliot.twisted import DeferredContext

from twisted.internet.defer import (
    Deferred, maybeDeferred, gatherResults, succeed,
)
from twisted.internet import task
from twisted.application.service import MultiService
from twisted.application.internet import TimerService
from twisted.python.usage import Options as _Options, UsageError
from twisted.python.filepath import FilePath
from twisted.python.url import URL
from twisted.web.client import Agent

from prometheus_client import Gauge

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceRegion
from txaws.route53.model import (
    Name, CNAME, RRSetKey, RRSet, delete_rrset, create_rrset, upsert_rrset,
)

from lae_util.service import AsynchronousService
from lae_util.eliot_destination import (
    opt_eliot_destination,
    eliot_logging_service,
)
from lae_util import opt_metrics_port

from .model import DeploymentConfiguration
from .subscription_manager import Client as SMClient
from .containers import (
    CONTAINERIZED_SUBSCRIPTION_VERSION,
    CUSTOMER_METADATA_LABELS,
    autopad_b32decode,
    configmap_name,
    deployment_name,
    configmap_public_host,
    create_configuration,
    create_deployment,
    new_service,
)
from .kubeclient import KubeClient, And, LabelSelector, NamespaceSelector

from txkube import (
    network_kubernetes, authenticate_with_serviceaccount,
    network_kubernetes_from_context,
)


def _kubernetes_from_environ(environ):
    try:
        host = environ[u"KUBERNETES_SERVICE_HOST"]
        port = int(environ[u"KUBERNETES_SERVICE_PORT"])
    except (KeyError, ValueError):
        return None
    return u"https://{}:{}/".format(host, port).encode("ascii")


class KubernetesClientOptionsMixin(object):
    optParameters = [
        ("kubernetes", None, _kubernetes_from_environ(environ),
         "The root URL of the Kubernetes API server.",
        ),

        ("k8s-context", None, None, "Use a kubectl configuration context to find Kubernetes."),

        ("k8s-config", None, None, "The path of a kubectl configuration file in which to find the context.", FilePath),

        ("kubernetes-namespace", None, None,
         "The Kubernetes namespace in which to perform convergence.",
        ),

    ]

    optFlags = [
        ("k8s-service-account", None, "Use a Kubernetes service account to authenticate to Kubernetes."),
    ]


    def get_kubernetes_service(self, reactor):
        if self["k8s-service-account"]:
            return network_kubernetes(
                # XXX is this really the url to use?
                base_url=URL.fromText(self["kubernetes"].decode("ascii")),
                agent=authenticate_with_serviceaccount(reactor),
            )
        return network_kubernetes_from_context(reactor, self["k8s-context"], self["k8s-config"])

    def postOptions(self):
        if self["kubernetes-namespace"] is None:
            raise UsageError("--kubernetes-namespace is required")
        if (self["k8s-context"] is None) == (not self["k8s-service-account"]):
            raise UsageError("Exactly one of --k8s-context or --k8s-service-account is required")
        if self["k8s-service-account"]:
            if self["kubernetes"] is None:
                raise UsageError("--kubernetes is required with --k8s-service-account")



@opt_metrics_port
class Options(_Options, KubernetesClientOptionsMixin):
    optParameters = [
        ("domain", None, None,
         "The domain on which the service is running "
         "(useful for alternate staging deployments).",
        ),

        ("endpoint", None, None, "The root URL of the subscription manager service."),

        ("aws-access-key-id-path", None, None, "The path of a file containing the AWS key identifier to use."),
        ("aws-secret-access-key-path", None, None, "The path of a file containing the AWS secret key to use."),

        # XXX This should be part of the product description.
        ("introducer-image", None, None, "The Docker image to run a Tahoe-LAFS introducer."),
        ("storageserver-image", None, None, "The Docker image to run a Tahoe-LAFS storage server."),

        ("interval", None, 10.0, "The interval (in seconds) at which to iterate on convergence.", float),

        ("log-gatherer-furl", None, None,
         "A fURL pointing at a Foolscap log gatherer where Tahoe-LAFS nodes should ship their logs.",
        ),
        ("stats-gatherer-furl", None, None,
         "A fURL pointing at a Foolscap stats gatherer where Tahoe-LAFS nodes should ship their stats.",
        ),
    ]

    opt_eliot_destination = opt_eliot_destination

    def postOptions(self):
        KubernetesClientOptionsMixin.postOptions(self)
        if self["domain"] is None:
            raise UsageError("--domain is required")
        self["domain"] = self["domain"].strip()
        if self["endpoint"] is None:
            raise UsageError("--endpoint is required")
        if self["endpoint"].endswith("/"):
            self["endpoint"] = self["endpoint"][:-1]



def makeService(options):
    # Boo global reactor
    # https://twistedmatrix.com/trac/ticket/9063
    from twisted.internet import reactor

    parent = MultiService()

    eliot_logging_service(
        reactor,
        options.get("destinations", []),
    ).setServiceParent(parent)

    options.get_metrics_service(reactor).setServiceParent(parent)

    agent = Agent(reactor)
    subscription_client = SMClient(
        endpoint=options["endpoint"],
        agent=agent,
        cooperator=task,
    )

    kubernetes = options.get_kubernetes_service(reactor)

    def get_k8s_client():
        d = kubernetes.versioned_client()
        d.addCallback(
            _finish_convergence_service,
            options,
            subscription_client,
            reactor,
        )
        return d

    AsynchronousService(
        get_k8s_client
    ).setServiceParent(parent)

    return parent



_CONVERGE_COMPLETE = Gauge(
    u"s4_last_convergence_succeeded",
    u"The time at which the convergence loop last succeeded.",
)



def _finish_convergence_service(
    k8s_client, options, subscription_client, reactor,
):
    k8s = KubeClient(k8s=k8s_client)

    access_key_id = FilePath(options["aws-access-key-id-path"]).getContent().strip()
    secret_access_key = FilePath(options["aws-secret-access-key-path"]).getContent().strip()

    aws = AWSServiceRegion(creds=AWSCredentials(
        access_key=access_key_id,
        secret_key=secret_access_key,
    ))

    Message.log(
        event=u"convergence-service:key-notification",
        key_id=access_key_id.decode("ascii"),
        secret_key_hash=sha256(secret_access_key).hexdigest().decode("ascii"),
    )

    config = DeploymentConfiguration(
        domain=options["domain"].decode("ascii"),
        kubernetes_namespace=options["kubernetes-namespace"].decode("ascii"),
        subscription_manager_endpoint=URL.fromText(options["endpoint"].decode("ascii")),

        s3_access_key_id=access_key_id.decode("ascii"),
        s3_secret_key=secret_access_key.decode("ascii"),

        introducer_image=options["introducer-image"].decode("ascii"),
        storageserver_image=options["storageserver-image"].decode("ascii"),

        log_gatherer_furl=options["log-gatherer-furl"],
        stats_gatherer_furl=options["stats-gatherer-furl"],
    )

    return _convergence_service(
        reactor,
        options["interval"],
        config,
        subscription_client,
        k8s,
        aws,
    )



def _convergence_service(reactor, interval, config, subscription_client, k8s, aws):
    def monitorable_converge(*a, **kw):
        d = converge(*a, **kw)
        def finished(passthrough):
            _CONVERGE_COMPLETE.set(reactor.seconds())
            return passthrough
        d.addCallback(finished)
        return d

    safe_converge = divert_errors_to_log(
        monitorable_converge, u"subscription_converger",
    )

    service = TimerService(
        interval,
        safe_converge,
        config,
        subscription_client,
        k8s,
        aws,
    )
    service.clock = reactor
    return service



def divert_errors_to_log(f, scope):
    def g(*a, **kw):
        action = start_action(action_type=scope + u":" + f.__name__)
        with action.context():
            d = DeferredContext(maybeDeferred(f, *a, **kw))
            d.addErrback(write_failure)
            return d.addActionFinish()
    return g


def with_action(action_type):
    def wrapper(f):
        def g(*args, **kwargs):
            action = start_action(action_type=action_type)
            with action.context():
                try:
                    result = f(*args, **kwargs)
                except Exception as e:
                    action.finish(e)
                else:
                    if isinstance(result, Deferred):
                        d = DeferredContext(result)
                        return d.addActionFinish()
                    else:
                        action.finish()
                        return result
        return g
    return wrapper


def _s4_selector(namespace):
    return And([
        LabelSelector(CUSTOMER_METADATA_LABELS),
        NamespaceSelector(namespace),
    ])


def get_customer_grid_service(k8s, namespace):
    action = start_action(action_type=u"load-services")
    with action.context():
        d = DeferredContext(k8s.get_services(_s4_selector(namespace)))
        def got_services(services):
            services = list(services)
            action.add_success_fields(service_count=len(services))
            if services:
                return services[0]
            return None
        d.addCallback(got_services)
        return d.addActionFinish()



def get_customer_grid_configmaps(k8s, namespace):
    action = start_action(action_type=u"load-configmaps")
    with action.context():
        d = DeferredContext(k8s.get_configmaps(_s4_selector(namespace)))
        def got_configmaps(configmaps):
            configmaps = list(configmaps)
            action.add_success_fields(configmap_count=len(configmaps))
            return configmaps
        d.addCallback(got_configmaps)
        return d.addActionFinish()



_DEPLOYMENTS = Gauge(
    u"s4_deployment_gauge",
    u"Current S4 Subscription Deployments",
)



def get_customer_grid_deployments(k8s, namespace):
    action = start_action(action_type=u"load-deployments")
    with action.context():
        d = DeferredContext(k8s.get_deployments(_s4_selector(namespace)))
        def got_deployments(deployments):
            deployments = list(deployments)
            action.add_success_fields(deployment_count=len(deployments))
            _DEPLOYMENTS.set(len(deployments))
            return deployments
        d.addCallback(got_deployments)
        return d.addActionFinish()



def get_customer_grid_replicasets(k8s, namespace):
    action = start_action(action_type=u"load-replicasets")
    with action.context():
        d = DeferredContext(k8s.get_replicasets(_s4_selector(namespace)))
        def got_replicasets(replicasets):
            replicasets = list(replicasets)
            action.add_success_fields(replicaset_count=len(replicasets))
            return replicasets
        d.addCallback(got_replicasets)
        return d.addActionFinish()



_PODS = Gauge(
    u"s4_pod_gauge",
    u"Current S4 Subscription Pods",
)

_PODS_RUNNING = Gauge(
    u"s4_running_pod_gauge",
    u"Current S4 Subscription Pods in Running state",
)


def count(iterator):
    return sum(1 for x in iterator)


def get_customer_grid_pods(k8s, namespace):
    action = start_action(action_type=u"load-pods")
    with action.context():
        d = DeferredContext(k8s.get_pods(_s4_selector(namespace)))
        def got_pods(pods):
            pods = list(pods)
            running = count(pod for pod in pods if pod.status.phase == u"Running")
            action.add_success_fields(pod_count=len(pods), pod_running_count=running)
            _PODS.set(len(pods))
            _PODS_RUNNING.set(running)
            return pods
        d.addCallback(got_pods)
        return d.addActionFinish()



def get_active_subscriptions(subscriptions):
    action = start_action(action_type=u"load-subscriptions")
    with action.context():
        d = DeferredContext(subscriptions.list())
        def got_subscriptions(subscriptions):
            subscriptions = list(subscriptions)
            action.add_success_fields(subscription_count=len(subscriptions))
            return {
                subscription.subscription_id: subscription
                for subscription
                in subscriptions
            }
        d.addCallback(got_subscriptions)
        return d.addActionFinish()



class _ZoneState(PClass):
    zone = field()
    rrsets = field()



class _State(PClass):
    subscriptions = field()
    configmaps = field()
    deployments = field()
    replicasets = field()
    pods = field()
    service = field()
    zone = field(type=_ZoneState)



def _get_converge_inputs(config, subscriptions, k8s, aws):
    a = start_action(action_type=u"load-converge-inputs")
    with a.context():
        d = DeferredContext(
            gatherResults([
                get_active_subscriptions(subscriptions),
                get_customer_grid_configmaps(k8s, config.kubernetes_namespace),
                get_customer_grid_deployments(k8s, config.kubernetes_namespace),
                get_customer_grid_replicasets(k8s, config.kubernetes_namespace),
                get_customer_grid_pods(k8s, config.kubernetes_namespace),
                get_customer_grid_service(k8s, config.kubernetes_namespace),
                get_hosted_zone_by_name(aws.get_route53_client(), Name(config.domain)),
            ]),
        )
        d.addCallback(
            lambda state: _State(**dict(
                zip([
                    u"subscriptions",
                    u"configmaps",
                    u"deployments",
                    u"replicasets",
                    u"pods",
                    u"service",
                    u"zone",
                ], state,
                ),
            )),
        )
        return d.addActionFinish()


@with_action(action_type=u"converge-logic")
def _converge_logic(actual, config, subscriptions, k8s, aws):
    convergers = [
        _converge_service,
        _converge_configmaps,
        _converge_deployments,
        _converge_replicasets,
        _converge_pods,
        _converge_route53_customer,
        _converge_route53_infrastructure,
    ]

    jobs = []
    for converger in convergers:
        with start_action(action_type=converger.func_name):
            jobs.extend(converger(actual, config, subscriptions, k8s, aws))

    return jobs



class _Changes(PClass):
    create = field()
    delete = field()



def _compute_changes(desired, actual):
    """
    Determine what changes to ``actual`` are necessary to agree with
    ``desired``.
    """
    # Start with the assumption that everything will need to be created.
    to_create = set(sorted(desired.iterkeys()))
    to_delete = set()

    # Visit everything in the actual state and determine if it needs to be
    # changed somehow.  Since we started with the assumption that everything
    # will need to be created (`to_create` initial value), anything that's
    # completely missing from the actual state will automatically get created.
    for sid in actual.itersubscription_ids():
        # Check to see if it is desired.
        try:
            subscription = desired[sid]
        except KeyError:
            # If it was missing, we don't want it.  Please delete it.  We
            # don't need to remove it from to_create because if it wasn't
            # desired, it won't have been there to begin with.
            Message.log(condition=u"undesired", subscription=sid)
            to_delete.add(sid)
            continue

        if actual.needs_update(subscription):
            # Something about the actual state disagrees with the subscription
            # state.  Delete the current state and re-create the new state.
            Message.log(condition=u"needs-update", subscription=sid)
            to_delete.add(sid)
        else:
            # It appears to be fine as-is.  Don't delete it and don't create
            # it.
            to_create.remove(sid)

    return _Changes(
        create=list(desired[sid] for sid in to_create),
        delete=to_delete,
    )



def _converge_service(actual, config, subscriptions, k8s, aws):
    create_service = (actual.service is None)
    if create_service:
        service = new_service(config.kubernetes_namespace, k8s.k8s.model)
        # Create it if it was missing.
        return [lambda: k8s.create(service)]

    return []



class _ChangeableDeployments(PClass):
    deploy_config = field(type=DeploymentConfiguration)
    deployments = field(
        factory=lambda deployments: {
            d.metadata.annotations[u"subscription"]: d
            for d in deployments
        },
    )

    def itersubscription_ids(self):
        return sorted(self.deployments.iterkeys())


    def needs_update(self, subscription):
        deployment = self.deployments[subscription.subscription_id]
        introducer = list(
            container
            for  container
            in deployment.spec.template.spec.containers
            if container.name == u"introducer"
        )[0]
        storageserver = list(
            container
            for  container
            in deployment.spec.template.spec.containers
            if container.name == u"storageserver"
        )[0]
        intro_port = introducer.ports[0].containerPort
        storage_port = storageserver.ports[0].containerPort

        return (
            subscription.introducer_port_number != intro_port or
            subscription.storage_port_number != storage_port
        ) or (
            self.deploy_config.introducer_image != introducer.image or
            self.deploy_config.storageserver_image != storageserver.image
        ) or (
            deployment.metadata.labels.version != CONTAINERIZED_SUBSCRIPTION_VERSION
        )



def _converge_deployments(actual, deploy_config, subscriptions, k8s, aws):
    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableDeployments(
            deploy_config=deploy_config,
            deployments=actual.deployments,
        ),
    )
    def delete(sid):
        return k8s.delete(k8s.k8s.model.v1beta1.Deployment(
            metadata=dict(
                namespace=deploy_config.kubernetes_namespace,
                name=deployment_name(sid),
            ),
        ))
    def create(subscription):
        deployment = create_deployment(deploy_config, subscription, k8s.k8s.model)
        return k8s.create(deployment)

    deletes = list(partial(delete, sid) for sid in changes.delete)
    creates = list(partial(create, s) for s in changes.create)
    return deletes + creates


def _converge_replicasets(actual, config, subscriptions, k8s, aws):
    # We don't ever have to create a ReplicaSet.  We'll just delete the ones
    # we don't need anymore.
    deletes = []
    for replicaset in actual.replicasets:
        sid = replicaset.metadata.annotations[u"subscription"]
        if sid not in actual.subscriptions:
            Message.log(condition=u"undesired", subscription=sid)
            deletes.append(replicaset.metadata)

    def delete(metadata):
        return k8s.delete(k8s.k8s.model.v1beta1.ReplicaSet(metadata=metadata))

    return list(partial(delete, metadata) for metadata in deletes)


def _converge_pods(actual, config, subscriptions, k8s, aws):
    # We don't ever have to create a Pod.  We'll just delete the ones we don't
    # need anymore.
    deletes = []
    for pod in actual.pods:
        sid = pod.metadata.annotations[u"subscription"]
        if sid not in actual.subscriptions:
            Message.log(condition=u"undesired", subscription=sid)
            deletes.append(pod.metadata)

    def delete(metadata):
        return k8s.delete(k8s.k8s.model.v1.Pod(metadata=metadata))

    return list(partial(delete, metadata) for metadata in deletes)


class _ChangeableConfigMaps(PClass):
    deploy_config = field(type=DeploymentConfiguration)
    k8s_model = field()
    configmaps = field(
        factory=lambda configmaps: {
            c.metadata.annotations[u"subscription"]: c
            for c in configmaps
        },
    )

    def itersubscription_ids(self):
        return sorted(self.configmaps.iterkeys())


    def needs_update(self, subscription):
        actual_configmap = self.configmaps[subscription.subscription_id]
        expected_configmap = create_configuration(
            self.deploy_config,
            subscription,
            self.k8s_model,
        )
        return actual_configmap.data != expected_configmap.data



def _converge_configmaps(actual, deploy_config, subscriptions, k8s, aws):
    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableConfigMaps(
            deploy_config=deploy_config,
            k8s_model=k8s.k8s.model,
            configmaps=actual.configmaps,
        ),
    )
    def delete(sid):
        return k8s.delete(k8s.k8s.model.v1.ConfigMap(
            metadata=dict(
                namespace=deploy_config.kubernetes_namespace,
                name=configmap_name(sid),
            ),
        ))
    def create(subscription):
        return k8s.create(create_configuration(deploy_config, subscription, k8s.k8s.model))
    deletes = list(partial(delete, sid) for sid in changes.delete)
    creates = list(partial(create, s) for s in changes.create)
    return deletes + creates



def _introducer_domain(domain):
    """
    Construct a ``Name`` for the intermediate domain name that glues per-user
    domain names to the load balancer hostname/address for the grid service.
    """
    return Name(u"introducer.{}".format(domain))



class _ChangeableZone(PClass):
    zone = field()
    rrsets = field()
    domain = field()

    def itersubscription_ids(self):
        for key in self.rrsets:
            if key.type == u"CNAME":
                subscription_part, rest = key.label.text.split(u".", 1)
                # XXX Ugh strings
                if Name(rest) == _introducer_domain(self.domain):
                    subscription_id = autopad_b32decode(subscription_part)
                    yield subscription_id


    def needs_update(self, subscription):
        # They all point to the same thing right now.  If it exists at all it
        # must be right.
        return False



def _converge_route53_customer(actual, config, subscriptions, k8s, aws):
    """
    Converge on the desired Route53 state relating to individual S4
    subscriptions.

    Specifically, make sure the per-subscription domain names that grants
    access to individual subscription's introducer and storage servers exist
    and point at the right thing.  Also clean up the names for any cancelled
    subscriptions.
    """
    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableZone(
            zone=actual.zone.zone,
            rrsets=actual.zone.rrsets,
            domain=config.domain,
        ),
    )
    # XXX Probably would be nice to group changes.  Also, issue changes insert
    # of delete/create.  Some structured objects would make that easier.
    route53 = aws.get_route53_client()
    def delete(sid):
        return delete_route53_rrsets(route53, actual.zone.zone, [sid])
    def create(subscription):
        return create_route53_rrsets(route53, actual.zone.zone, [subscription])
    deletes = list(partial(delete, sid) for sid in changes.delete)
    creates = list(partial(create, s) for s in changes.create)

    Message.log(
        event=u"convergence-service:route53-customer",
        create=list(subscription.subscription_id for subscription in changes.create),
        delete=list(changes.delete),
    )
    return deletes + creates



def _converge_route53_infrastructure(actual, config, subscriptions, k8s, aws):
    """
    Converge on the desired Route53 state relating to general S4
    infrastructure.

    Specifically, make sure there is an rrset for the ``introducer`` subdomain
    which points at the customer grid service's load balancer endpoint.
    """
    if actual.service is None or actual.service.status is None:
        # Cannot do anything without a v1.Service or one without a populated
        # v1.ServiceStatus field.
        return []

    if not actual.service.status.loadBalancer.ingress:
        # Also cannot do anything if we don't yet know what our ingress
        # address is.
        return []

    loadbalancer_hostname = actual.service.status.loadBalancer.ingress[0].hostname
    introducer_key = RRSetKey(label=_introducer_domain(config.domain), type=u"CNAME")
    desired_rrset = RRSet(
        label=introducer_key.label,
        type=introducer_key.type,
        ttl=60,
        records={
            CNAME(canonical_name=Name(loadbalancer_hostname)),
        },
    )

    actual_rrset = actual.zone.rrsets.get(introducer_key, None)
    if actual_rrset == desired_rrset:
        # Nothing to do.
        return []

    # Create it or change it to what we want.
    route53 = aws.get_route53_client()
    return [
        lambda: change_route53_rrsets(route53, actual.zone.zone, desired_rrset),
    ]


def _execute_converge_output(jobs):
    if not jobs:
        return succeed(None)

    a = start_action(action_type=u"execute-converge-step")
    with a.context():
        job = jobs.pop(0)
        d = DeferredContext(job())
        d.addErrback(write_failure)
        d = d.addActionFinish()

    if jobs:
        # Capture whatever action context is active now and make sure it is
        # also active when we get back here to process the next job.
        DeferredContext(d).addCallback(
            lambda ignored: _execute_converge_output(jobs),
        )
    return d


def _execute_converge_outputs(jobs):
    a = start_action(action_type=u"execute-converge-steps")
    with a.context():
        d = DeferredContext(_execute_converge_output(jobs))
        return d.addActionFinish()


def converge(config, subscriptions, k8s, aws):
    """
    Bring provisioned resources in line with active subscriptions.

    :param DeploymentConfig config: S4-global configuration necessary for
        provisioning resources.

    :param subscription_manager.Client subscription_manager: A client for
        interrogating the subscriptions database.

    :param txkube.IKubernetesClient k8s: A client for interacting with
        Kubernetes.

    :param AWSServiceRegion aws: A client for interacting with AWS.

    :return Deferred(NoneType): The returned ``Deferred`` fires after one
        attempt has been made to bring the actual state of provisioned
        resources in line with the desired state of provisioned resources
        based on the currently active subscriptions.
    """
    # Create and destroy deployments as necessary.  Use the
    # subscription manager to find out what subscriptions are active
    # and use look at the Kubernetes configuration to find out what
    # subscription-derived deployments exist.  Also detect port
    # mis-configurations and correct them.
    a = start_action(action_type=u"converge")
    with a.context():
        d = DeferredContext(_get_converge_inputs(config, subscriptions, k8s, aws))
        d.addCallback(_converge_logic, config, subscriptions, k8s, aws)
        d.addCallback(_execute_converge_outputs)
        d.addCallback(lambda result: None)
        return d.addActionFinish()



@with_action(action_type=u"find-zones")
def get_hosted_zone_by_name(route53, name):
    """
    Get a ``HostedZone`` with a zone name matching ``name``.

    :param route53: A txaws Route53 client.

    :param txaws.route53.model.Name name: The zone name to look for.

    :raise KeyError: If no matching hosted zone is found.

    :return Deferred(HostedZone): The hosted zone with a matching name.
    """
    action = start_action(action_type=u"get-hosted-zone")
    with action.context():
        d = DeferredContext(route53.list_hosted_zones())
        def filter_results(zones):
            Message.log(zone_names=list(zone.name for zone in zones))
            for zone in zones:
                # XXX Bleuch zone.name should be a Name!
                if Name(zone.name) == name:
                    d = _load_all_rrsets(route53, zone.identifier)
                    d.addCallback(
                        lambda rrsets, zone=zone: _ZoneState(
                            zone=zone,
                            rrsets=rrsets,
                        ),
                    )
                    return d
            raise KeyError(name)
        d.addCallback(filter_results)
        return d.addActionFinish()



def _load_all_rrsets(route53, zone_identifier, name=None, type=None, accum=None):
    """
    Load all rrsets for a given zone, collecting multiple pages of results if
    necessary.
    """
    a = start_action(event=u"load-all-rrsets")
    with a.context():
        if accum is None:
            accum = {}
        d = DeferredContext(
            route53.list_resource_record_sets(
                zone_id=zone_identifier,
                # Make sure we know how many results to expect.  This is the
                # default and maximum allowed item limit, though.
                maxitems=100,
                # Start the page at the rrset identified by these two values.
                name=name,
                type=type,
            ),
        )
        def got_some_rrsets(rrsets):
            before_update = len(rrsets)
            accum.update(rrsets)
            after_update = len(rrsets)
            if len(rrsets) < 100 or before_update == after_update:
                # Fewer results than we asked for means we must be on the last
                # page.  Also if everything we got we already had, we're not
                # making progress so we should stop.
                return accum

            # Otherwise, ask for the next page.  We do this slightly wrong, using
            # max(rrsets) as the starting key because txaws does not give us
            # access to the correct values from the response -
            # NextRecordIdentifier and NextRecordType.  This just means we'll load
            # one duplicate item on each page.  They all go into the dict so it
            # doesn't affect correctness.
            maxkey = max(rrsets)
            # Make sure we also preserve the Eliot context for callbacks of
            # this next Deferred.
            d = DeferredContext(_load_all_rrsets(
                route53,
                zone_identifier,
                name=maxkey.label,
                type=maxkey.type,
                accum=accum,
            ))
            return d.addActionFinish()
        d.addCallback(got_some_rrsets)
        return d.addActionFinish()



def _introducer_name_for_subscription(subscription_id, domain):
    return Name(configmap_public_host(subscription_id, domain))



def _cname_for_subscription(domain):
    return CNAME(
        Name(
            u"introducer.{domain}".format(
                domain=domain,
            )
        )
    )

def _rrset_for_subscription(subscription_id, zone_name):
    return RRSet(
        label=_introducer_name_for_subscription(subscription_id, zone_name),
        type=u"CNAME",
        ttl=60,
        records={_cname_for_subscription(zone_name)},
    )


def delete_route53_rrsets(route53, zone, subscription_ids):
    a = start_action(action_type=u"delete-route53", subscription_ids=list(subscription_ids))
    with a.context():
        d = route53.change_resource_record_sets(zone.identifier, list(
            delete_rrset(_rrset_for_subscription(subscription_id, zone.name))
            for subscription_id
            in subscription_ids
        ))
        d = DeferredContext(d)
        return d.addActionFinish()


def create_route53_rrsets(route53, zone, subscriptions):
    a = start_action(action_type=u"create-route53")
    with a.context():
        d = route53.change_resource_record_sets(zone.identifier, list(
            create_rrset(_rrset_for_subscription(subscription.subscription_id, zone.name))
            for subscription
            in subscriptions
        ))
        d = DeferredContext(d)
        return d.addActionFinish()


def change_route53_rrsets(route53, zone, rrset):
    a = start_action(action_type=u"change-route53", zone=zone.identifier, rrset=attr.asdict(rrset))
    with a.context():
        d = route53.change_resource_record_sets(zone.identifier, [upsert_rrset(rrset)])
        d = DeferredContext(d)
        return d.addActionFinish()
