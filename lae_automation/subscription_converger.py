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
from twisted.application.internet import TimerService
from twisted.python.usage import Options as _Options, UsageError
from twisted.python.filepath import FilePath
from twisted.python.url import URL
from twisted.web.client import Agent

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceRegion
from txaws.route53.model import (
    Name, CNAME, RRSetKey, RRSet, delete_rrset, create_rrset, upsert_rrset,
)

from .model import DeploymentConfiguration
from .subscription_manager import Client as SMClient
from .containers import (
    autopad_b32decode,
    configmap_name, deployment_name,
    configmap_public_host,
    create_configuration, create_deployment,
    new_service,
)
from .initialize import create_user_bucket
from .signup import get_bucket_name
from .kubeclient import KubeClient, And, LabelSelector, NamespaceSelector

from txkube import (
    v1, v1beta1,
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
    ]

    def __init__(self):
        _Options.__init__(self)
        self["eliot-to-stdout"] = True

    def opt_no_eliot_to_stdout(self):
        """
        Do not dump Eliot logs to stdout.
        """
        self["eliot-to-stdout"] = False

    def postOptions(self):
        KubernetesClientOptionsMixin.postOptions(self)
        if self["domain"] is None:
            raise UsageError("--domain is required")
        if self["endpoint"] is None:
            raise UsageError("--endpoint is required")
        if self["endpoint"].endswith("/"):
            self["endpoint"] = self["endpoint"][:-1]


def makeService(options):
    # Boo global reactor
    # https://twistedmatrix.com/trac/ticket/9063
    from twisted.internet import reactor
    agent = Agent(reactor)
    subscription_client = SMClient(endpoint=options["endpoint"], agent=agent, cooperator=task)

    kubernetes = options.get_kubernetes_service(reactor)

    k8s_client = kubernetes.client()
    k8s = KubeClient(k8s=k8s_client)

    access_key_id = FilePath(options["aws-access-key-id-path"]).getContent().strip()
    secret_access_key = FilePath(options["aws-secret-access-key-path"]).getContent().strip()

    aws = AWSServiceRegion(creds=AWSCredentials(
        access_key=access_key_id,
        secret_key=secret_access_key,
    ))

    if options["eliot-to-stdout"]:
        # XXX not exactly the right place for this
        from eliot import to_file
        from sys import stdout
        to_file(stdout)

    Message.log(
        event=u"convergence-service:key-notification",
        key_id=access_key_id.decode("ascii"),
        secret_key_hash=sha256(secret_access_key).hexdigest().decode("ascii"),
    )

    # XXX I get to leave a ton of fields empty because I happen to know
    # they're not used in this codepath. :/ Maybe this suggests something has
    # gone wrong ...
    config = DeploymentConfiguration(
        domain=options["domain"].decode("ascii"),
        kubernetes_namespace=options["kubernetes-namespace"].decode("ascii"),
        subscription_manager_endpoint=URL.fromText(options["endpoint"].decode("ascii")),

        products=[{}],
        s3_access_key_id=access_key_id.decode("ascii"),
        s3_secret_key=secret_access_key.decode("ascii"),

        introducer_image=options["introducer-image"].decode("ascii"),
        storageserver_image=options["storageserver-image"].decode("ascii"),

        ssec2_access_key_id=None,
        ssec2_secret_path=None,

        ssec2admin_keypair_name=None,
        ssec2admin_privkey_path=None,

        monitor_pubkey_path=None,
        monitor_privkey_path=None,

        secretsfile=open(u"/dev/full", "w"),
        serverinfopath=None,

        log_gatherer_furl=None,
        stats_gatherer_furl=None,
    )

    return TimerService(
        options["interval"],
        divert_errors_to_log(converge, u"subscription_converger"), config, subscription_client, k8s, aws,
    )

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
                result = f(*args, **kwargs)
                if isinstance(result, Deferred):
                    d = DeferredContext(result)
                    d.addActionFinish()
                return result
        return g
    return wrapper


def _s4_selector(namespace):
    return And([
        LabelSelector(dict(
            provider=u"LeastAuthority",
            app=u"s4",
            component=u"customer-tahoe-lafs",
            version=u"1",
        )),
        # XXX Need to get this from configuration.
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
                # Work around
                # https://github.com/LeastAuthority/txkube/issues/94
                return v1.Service.create(services[0].serialize())
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



def get_customer_grid_deployments(k8s, namespace):
    action = start_action(action_type=u"load-deployments")
    with action.context():
        d = DeferredContext(k8s.get_deployments(_s4_selector(namespace)))
        def got_deployments(deployments):
            deployments = list(deployments)
            action.add_success_fields(deployment_count=len(deployments))
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



def get_customer_grid_pods(k8s, namespace):
    action = start_action(action_type=u"load-pods")
    with action.context():
        d = DeferredContext(k8s.get_pods(_s4_selector(namespace)))
        def got_pods(pods):
            pods = list(pods)
            action.add_success_fields(pod_count=len(pods))
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



def get_s3_buckets(s3):
    action = start_action(action_type=u"list-buckets")
    with action.context():
        d = DeferredContext(s3.list_buckets())
        def got_buckets(buckets):
            buckets = list(buckets)
            action.add_success_fields(bucket_count=len(buckets))
            return buckets
        d.addCallback(got_buckets)
        return d.addActionFinish()



class _State(PClass):
    subscriptions = field()
    configmaps = field()
    deployments = field()
    # replicasets = field()
    # pods = field()
    service = field()
    zone = field()
    buckets = field()



def _get_converge_inputs(config, subscriptions, k8s, aws):
    a = start_action(action_type=u"load-converge-inputs")
    with a.context():
        d = DeferredContext(
            gatherResults([
                get_active_subscriptions(subscriptions),
                get_customer_grid_configmaps(k8s, config.kubernetes_namespace),
                get_customer_grid_deployments(k8s, config.kubernetes_namespace),
                # get_customer_grid_replicasets(k8s, config.kubernetes_namespace),
                # get_customer_grid_pods(k8s, config.kubernetes_namespace),
                get_customer_grid_service(k8s, config.kubernetes_namespace),
                get_hosted_zone_by_name(aws.get_route53_client(), Name(config.domain)),
                get_s3_buckets(aws.get_s3_client()),
            ]),
        )
        d.addCallback(
            lambda state: _State(**dict(
                zip([
                    u"subscriptions",
                    u"configmaps",
                    u"deployments",
                    # u"replicasets",
                    # u"pods",
                    u"service",
                    u"zone",
                    u"buckets",
                ], state,
                ),
            )),
        )
        return d.addActionFinish()


@with_action(action_type=u"converge-logic")
def _converge_logic(actual, config, subscriptions, k8s, aws):
    convergers = [
        _converge_s3,
        _converge_service,
        _converge_configmaps,
        _converge_deployments,
        # _converge_replicasets,
        # _converge_pods,
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
            to_delete.add(sid)
            continue

        if actual.needs_update(subscription):
            # Something about the actual state disagrees with the subscription
            # state.  Delete the current state and re-create the new state.
            to_delete.add(sid)
        else:
            # It appears to be fine as-is.  Don't delete it and don't create
            # it.
            to_create.remove(sid)

    return _Changes(
        create=list(desired[sid] for sid in to_create),
        delete=to_delete,
    )



def _converge_s3(actual, config, subscription, k8s, aws):
    buckets = []
    actual_bucket_names = {bucket.name for bucket in actual.buckets}
    for subscription in actual.subscriptions.itervalues():
        bucket_name = get_bucket_name(
            subscription.subscription_id, subscription.customer_id,
        )
        if bucket_name not in actual_bucket_names:
            Message.log(actor=u"converge-s3", bucket=bucket_name, activity=u"create")
            buckets.append(bucket_name)

    s3 = aws.get_s3_client()
    from twisted.internet import reactor

    return list(
        lambda n=n: create_user_bucket(reactor, s3, n)
        for n
        in buckets
    )



def _converge_service(actual, config, subscriptions, k8s, aws):
    create_service = (actual.service is None)
    if create_service:
        service = new_service(config.kubernetes_namespace)
        # Create it if it was missing.
        return [lambda: k8s.create(service)]

    return []



class _ChangeableDeployments(PClass):
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
        intro = (
            deployment.spec.template.spec.containers[0].ports[0].containerPort
        )
        storage = (
            deployment.spec.template.spec.containers[1].ports[0].containerPort
        )

        return (
            subscription.introducer_port_number != intro or
            subscription.storage_port_number != storage
        )



def _converge_deployments(actual, config, subscriptions, k8s, aws):
    # XXX Oh boy there's two more deployment states to deal with. :/ Merely
    # deleting a deployment doesn't clean up its replicaset (nor its pod,
    # therefore).  So instead we need to update the deployment with replicas =
    # 0 and wait for it to settle, and only then delete it.
    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableDeployments(deployments=actual.deployments),
    )
    def delete(sid):
        return k8s.delete(v1beta1.Deployment(
            metadata=dict(
                namespace=config.kubernetes_namespace,
                name=deployment_name(sid),
            ),
        ))
    def create(subscription):
        return k8s.create(create_deployment(config, subscription))

    deletes = list(partial(delete, sid) for sid in changes.delete)
    creates = list(partial(create, s) for s in changes.create)
    return deletes + creates


# XXX Untested
def _converge_replicasets(actual, config, subscriptions, k8s, aws):
    # We don't ever have to create a ReplicaSet.  We'll just delete the ones
    # we don't need anymore.
    deletes = []
    for replicaset in actual.replicasets:
        if replicaset.metadata.labels[u"subscription"] not in actual.subscriptions:
            deletes.append(replicaset.metadata)

    def delete(metadata):
        return k8s.delete(v1beta1.ReplicaSet(metadata=metadata))

    return list(partial(delete, metadata) for metadata in deletes)


# XXX Untested
def _converge_pods(actual, config, subscriptions, k8s, aws):
    # We don't ever have to create a Pod.  We'll just delete the ones we don't
    # need anymore.

    deletes = []
    for pod in actual.pods:
        if pod.metadata.labels[u"subscription"] not in actual.subscriptions:
            deletes.append(pod.metadata)

    def delete(metadata):
        return k8s.delete(v1.Pod(metadata=metadata))

    return list(partial(delete, metadata) for metadata in deletes)


class _ChangeableConfigMaps(PClass):
    configmaps = field(
        factory=lambda configmaps: {
            c.metadata.annotations[u"subscription"]: c
            for c in configmaps
        },
    )

    def itersubscription_ids(self):
        return sorted(self.configmaps.iterkeys())


    def needs_update(self, subscription):
        # TODO
        return False



def _converge_configmaps(actual, config, subscriptions, k8s, aws):
    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableConfigMaps(configmaps=actual.configmaps),
    )
    def delete(sid):
        return k8s.delete(v1.ConfigMap(
            metadata=dict(
                namespace=config.kubernetes_namespace,
                name=configmap_name(sid),
            ),
        ))
    def create(subscription):
        return k8s.create(create_configuration(config, subscription))
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
            zone=actual.zone[0],
            rrsets=actual.zone[1],
            domain=config.domain,
        ),
    )
    # XXX Probably would be nice to group changes.  Also, issue changes insert
    # of delete/create.  Some structured objects would make that easier.
    route53 = aws.get_route53_client()
    def delete(sid):
        return delete_route53_rrsets(route53, actual.zone[0], [sid])
    def create(subscription):
        return create_route53_rrsets(route53, actual.zone[0], [subscription])
    deletes = list(partial(delete, sid) for sid in changes.delete)
    creates = list(partial(create, s) for s in changes.create)
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

    zone, rrsets = actual.zone
    actual_rrset = rrsets.get(introducer_key, None)
    if actual_rrset == desired_rrset:
        # Nothing to do.
        return []

    # Create it or change it to what we want.
    route53 = aws.get_route53_client()
    return [
        lambda: change_route53_rrsets(route53, zone, desired_rrset),
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
        d.addCallback(lambda ignored: _execute_converge_output(jobs))
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
                    d = route53.list_resource_record_sets(zone_id=zone.identifier)
                    d.addCallback(lambda rrsets: (zone, rrsets))
                    return d
            raise KeyError(name)
        d.addCallback(filter_results)
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
