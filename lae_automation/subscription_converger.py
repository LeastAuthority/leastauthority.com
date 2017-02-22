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

from pyrsistent import PClass, field


from eliot import Message, start_action, write_failure
from eliot.twisted import DeferredContext

from twisted.internet.defer import Deferred, inlineCallbacks, returnValue, maybeDeferred, gatherResults
from twisted.internet import task
from twisted.application.internet import TimerService
from twisted.python.usage import Options as _Options, UsageError
from twisted.python.filepath import FilePath
from twisted.python.url import URL
from twisted.web.client import Agent

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceRegion
from txaws.route53.model import Name, CNAME, RRSet, delete_rrset, create_rrset

from .model import DeploymentConfiguration
from .subscription_manager import Client as SMClient
from .containers import (
    SUBSCRIPTION_ANNOTATION_PREFIX,
    autopad_b32decode,
    annotation_key_for_sid,
    sid_for_annotation_key,
    configmap_name, deployment_name,
    configmap_public_host,
    create_configuration, create_deployment,
    new_service,
    add_subscription_to_service, remove_subscription_from_service
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


class Options(_Options):
    optParameters = [
        ("domain", None, None,
         "The domain on which the service is running "
         "(useful for alternate staging deployments).",
        ),

        ("kubernetes-namespace", None, None,
         "The Kubernetes namespace in which to perform convergence.",
        ),

        ("endpoint", None, None, "The root URL of the subscription manager service."),
        ("k8s-context", None, None, "Use a kubectl configuration context to find Kubernetes."),
        ("k8s-config", None, None, "The path of a kubectl configuration file in which to find the context.", FilePath),

        ("aws-access-key-id-path", None, None, "The path of a file containing the AWS key identifier to use."),
        ("aws-secret-access-key-path", None, None, "The path of a file containing the AWS secret key to use."),

        # XXX This should be part of the product description.
        ("introducer-image", None, None, "The Docker image to run a Tahoe-LAFS introducer."),
        ("storageserver-image", None, None, "The Docker image to run a Tahoe-LAFS storage server."),

        ("interval", None, 10.0, "The interval (in seconds) at which to iterate on convergence.", float),

        ("kubernetes", None, _kubernetes_from_environ(environ), "The root URL of the Kubernetes API server."),
    ]

    optFlags = [
        ("k8s-service-account", None, "Use a Kubernetes service account to authenticate to Kubernetes."),
    ]

    def postOptions(self):
        if self["domain"] is None:
            raise UsageError("--domain is required")
        if self["kubernetes-namespace"] is None:
            raise UsageError("--kubernetes-namespace is required")
        if self["endpoint"] is None:
            raise UsageError("--endpoint is required")
        if self["endpoint"].endswith("/"):
            self["endpoint"] = self["endpoint"][:-1]
        if self["kubernetes"] is None:
            raise UsageError("--kubernetes is required")

        if (self["k8s-context"] is None) == (not self["k8s-service-account"]):
            raise UsageError("Exactly one of --k8s-context or --k8s-service-account is required")


def makeService(options):
    from twisted.internet import reactor
    agent = Agent(reactor)
    subscription_client = SMClient(endpoint=options["endpoint"], agent=agent, cooperator=task)

    if options["k8s-service-account"]:
        kubernetes = network_kubernetes(
            # XXX is this really the url to use?
            base_url=URL.fromText(options["kubernetes"].decode("ascii")),
            agent=authenticate_with_serviceaccount(reactor),
        )
    else:
        kubernetes = network_kubernetes_from_context(reactor, options["k8s-context"], options["k8s-config"])

    k8s_client = kubernetes.client()
    k8s = KubeClient(k8s=k8s_client)

    access_key_id = FilePath(options["aws-access-key-id-path"]).getContent().strip()
    secret_access_key = FilePath(options["aws-secret-access-key-path"]).getContent().strip()

    aws = AWSServiceRegion(creds=AWSCredentials(
        access_key=access_key_id,
        secret_key=secret_access_key,
    ))

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
        divert_errors_to_log(converge), config, subscription_client, k8s, aws,
    )

def divert_errors_to_log(f):
    def g(*a, **kw):
        action = start_action(action_type=u"subscription_converger:" + f.__name__)
        with action.context():
            d = DeferredContext(maybeDeferred(f, *a, **kw))
            d = d.addActionFinish()
            # The failure was logged by the above.  Now squash it.
            d.addErrback(lambda err: None)
            return d
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



@inlineCallbacks
def get_active_subscriptions(subscriptions):
    with start_action(action_type=u"load-subscriptions") as action:
        active_subscriptions = {
            subscription.subscription_id: subscription
            for subscription
            in (yield subscriptions.list())
        }
        action.add_success_fields(subscription_count=len(active_subscriptions))
    returnValue(active_subscriptions)



def get_s3_buckets(s3):
    return s3.list_buckets()



class _State(PClass):
    subscriptions = field()
    configmaps = field()
    deployments = field()
    service = field()
    zone = field()
    buckets = field()



def _get_converge_inputs(config, subscriptions, k8s, aws):
    return gatherResults([
        get_active_subscriptions(subscriptions),
        get_customer_grid_configmaps(k8s, config.kubernetes_namespace),
        get_customer_grid_deployments(k8s, config.kubernetes_namespace),
        get_customer_grid_service(k8s, config.kubernetes_namespace),
        get_hosted_zone_by_name(aws.get_route53_client(), Name(config.domain)),
        get_s3_buckets(aws.get_s3_client()),
    ]).addCallback(
        lambda state: _State(**dict(
            zip([
                u"subscriptions",
                u"configmaps",
                u"deployments",
                u"service",
                u"zone",
                u"buckets",
            ], state,
            ),
        ))
    )


@with_action(action_type=u"converge-logic")
def _converge_logic(actual, config, subscriptions, k8s, aws):
    convergers = [
        _converge_s3,
        _converge_service,
        _converge_configmaps,
        _converge_deployments,
        _converge_route53,
    ]

    jobs = []
    for converger in convergers:
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



class _ChangeableService(PClass):
    service = field()

    def itersubscription_ids(self):
        annotations = self.service.metadata.annotations
        for annotation, value in annotations.iteritems():
            if annotation.startswith(SUBSCRIPTION_ANNOTATION_PREFIX):
                sid = sid_for_annotation_key(annotation)
                yield sid


    def needs_update(self, subscription):
        sid = subscription.subscription_id
        # Just compare the annotation.  Assume we always atomically update the
        # annotation with the ports in the spec.
        key = annotation_key_for_sid(sid)
        value = self.service.metadata.annotations[key]
        version, rest = value.split(None, 1)
        if version != u"v1":
            raise Exception("Zoops")
        names = rest.split()

        # Get the result to have introducer then storage to simplify code
        # below.
        ports = sorted(
            (port for port in self.service.spec.ports if port.name in names),
            key=lambda p: names.index(p.name),
        )

        assert len(ports) == 2

        introducer = ports[0].port
        storage = ports[1].port

        return (
            subscription.introducer_port_number != introducer or
            subscription.storage_port_number != storage
        )



def _converge_s3(actual, config, subscription, k8s, aws):
    buckets = []
    for subscription in actual.subscriptions.itervalues():
        bucket_name = get_bucket_name(
            subscription.subscription_id, subscription.customer_id,
        )
        if bucket_name not in actual.buckets:
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
        configured_service = new_service(config.kubernetes_namespace)
    else:
        configured_service = actual.service

    changes = _compute_changes(
        actual.subscriptions,
        _ChangeableService(service=configured_service),
    )

    service = apply_service_changes(
        configured_service,
        to_delete=changes.delete,
        to_create=changes.create,
    )

    if create_service:
        # Always create it if it was missing, even if there are no
        # subscriptions.
        return [lambda: k8s.create(service)]
    else:
        if changes.create or changes.delete:
            # Only replace it if there were changes made.
            return [lambda: k8s.replace(service)]
    # Neither replacement nor creation needed, no jobs to execute.
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



class _ChangeableZone(PClass):
    zone = field()
    rrsets = field()
    domain = field()

    def itersubscription_ids(self):
        for key in self.rrsets:
            if key.type == u"CNAME":
                subscription_part, rest = key.label.text.split(u".", 1)
                # XXX Ugh strings
                if rest.rstrip(u".") == u"introducer.{}".format(self.domain).rstrip(u"."):
                    subscription_id = autopad_b32decode(subscription_part)
                    yield subscription_id


    def needs_update(self, subscription):
        # They all point to the same thing right now.  If it exists at all it
        # must be right.
        return False



def _converge_route53(actual, config, subscriptions, k8s, aws):
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



def _execute_converge_outputs(jobs):
    if not jobs:
        return

    job = jobs.pop(0)
    d = DeferredContext(job())
    d.addErrback(write_failure)
    if jobs:
        d.addCallback(lambda ignored: _execute_converge_outputs(jobs))
    return d.result



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
    d = route53.list_hosted_zones()
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
    return d



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
    c = DeferredContext(d)
    c.addActionFinish()
    return c.result

def create_route53_rrsets(route53, zone, subscriptions):
    a = start_action(action_type=u"create-route53")
    with a.context():
        d = route53.change_resource_record_sets(zone.identifier, list(
            create_rrset(_rrset_for_subscription(subscription.subscription_id, zone.name))
            for subscription
            in subscriptions
        ))
    c = DeferredContext(d)
    c.addActionFinish()
    return c.result


def apply_service_changes(service, to_delete, to_create):
    with start_action(action_type=u"change-service"):
        with_deletions = reduce(
            remove_subscription_from_service, to_delete, service,
        )
        with_creations = reduce(
            add_subscription_to_service, to_create, with_deletions,
        )
        return with_creations
