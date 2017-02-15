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

from base64 import b32encode
from os import environ

import attr

from eliot import Message, start_action
from eliot.twisted import DeferredContext

from twisted.internet.defer import Deferred, inlineCallbacks, returnValue, maybeDeferred
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
    configmap_name, deployment_name,
    create_configuration, create_deployment,
    new_service,
    add_subscription_to_service, remove_subscription_from_service
)
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
        ("endpoint", None, None, "The root URL of the subscription manager service."),
        ("k8s-context", None, None, "Use a kubectl configuration context to find Kubernetes."),
        ("k8s-config", None, None, "The path of a kubectl configuration file in which to find the context.", FilePath),

        ("aws-access-key-id-path", None, None, "The path of a file containing the AWS key identifier to use."),
        ("aws-secret-access-key-path", None, None, "The path of a file containing the AWS secret key to use."),

        ("interval", None, 10.0, "The interval (in seconds) at which to iterate on convergence.", float),

        ("kubernetes", None, _kubernetes_from_environ(environ), "The root URL of the Kubernetes API server."),
    ]

    optFlags = [
        ("k8s-service-account", None, "Use a Kubernetes service account to authenticate to Kubernetes."),
    ]

    def postOptions(self):
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
    from twisted.internet import task
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

    aws = AWSServiceRegion(creds=AWSCredentials(
        access_key=FilePath(options["aws-access-key-id-path"]).getContent(),
        secret_key=FilePath(options["aws-secret-access-key-path"]).getContent(),
    ))

    # XXX Exclusive for static attributes at this time ... really need to
    # break this up.
    config = DeploymentConfiguration(
        products=[{}],
        s3_access_key_id=None,
        s3_secret_key=None,
        amiimageid=None,
        instancesize=None,

        usertoken=None,
        producttoken=None,

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

    # XXX not exactly the right place for this
    from eliot import to_file
    from sys import stdout
    to_file(stdout)

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
            provider="LeastAuthority",
            app="s4",
            component="customer-tahoe-lafs"
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


@inlineCallbacks
def converge(config, subscriptions, k8s, aws):
    # Create and destroy deployments as necessary.  Use the
    # subscription manager to find out what subscriptions are active
    # and use look at the Kubernetes configuration to find out what
    # subscription-derived deployments exist.  Also detect port
    # mis-configurations and correct them.

    active_subscriptions = yield get_active_subscriptions(subscriptions)
    configured_deployments = yield get_customer_grid_deployments(k8s, config.kubernetes_namespace)
    configured_service = (yield get_customer_grid_service(k8s, config.kubernetes_namespace))

    create_service = (configured_service is None)
    if create_service:
        configured_service = new_service(config.kubernetes_namespace)

    to_create = set(active_subscriptions.iterkeys())
    to_delete = set()

    for deployment in configured_deployments:
        subscription_id = deployment.metadata.labels.subscription
        try:
            subscription = active_subscriptions[subscription_id]
        except KeyError:
            to_delete.add(subscription_id)
            continue

        to_create.remove(subscription_id)

        introducer_port = (
            deployment.spec.template.spec.containers[0].ports[0].containerPort
        )
        storage_port = (
            deployment.spec.template.spec.containers[1].ports[0].containerPort
        )
        if introducer_port != subscription.introducer_port_number:
            Message.log(modify_deployment=subscription_id, reason=u"introducer port mismatch")
            to_delete.add(subscription_id)
            to_create.add(subscription_id)
        elif storage_port != subscription.storage_port_number:
            Message.log(modify_deployment=subscription_id, reason=u"storage port mismatch")
            to_delete.add(subscription_id)
            to_create.add(subscription_id)

    to_create_subscriptions = list(active_subscriptions[sid] for sid in to_create)
    configmaps = list(
        create_configuration(config, details)
        for details
        in to_create_subscriptions
    )
    deployments = list(
        create_deployment(config, details)
        for details
        in to_create_subscriptions
    )
    service = apply_service_changes(configured_service, to_delete, to_create_subscriptions)

    route53 = aws.get_route53_client()
    try:
        zone = yield get_hosted_zone_by_name(route53, config.domain)
    except KeyError:
        raise ValueError("Configured domain {!r} does not exist in Route53.".format(config.domain))

    a = start_action(
        action_type=u"enacting",
        to_delete=list(to_delete),
        to_create=list(to_create),
    )
    # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # the objects have no namespace so they can't be created
    # also this function is now (I)/O -> logic -> I/(O)
    # so probably could be refactored now
    with a:
        yield delete_route53_rrsets(route53, zone, to_delete)
        for subscription_id in to_delete:
            yield k8s.delete(v1beta1.Deployment(
                metadata=dict(
                    name=deployment_name(subscription_id),
                    namespace=config.kubernetes_namespace,
                ),
            ))
            yield k8s.delete(v1.ConfigMap(
                metadata=dict(
                    name=configmap_name(subscription_id),
                    namespace=config.kubernetes_namespace,
                ),
            ))

        for configmap in configmaps:
            yield k8s.create(configmap)
        for deployment in deployments:
            yield k8s.create(deployment)
        if create_service:
            yield k8s.create(service)
        else:
            yield k8s.replace(service)
        yield create_route53_rrsets(route53, zone, to_create_subscriptions)


@with_action(action_type=u"find-zones")
def get_hosted_zone_by_name(route53, name):
    d = route53.list_hosted_zones()
    def filter_results(zones):
        Message.log(zone_names=list(zone.name for zone in zones))
        for zone in zones:
            if zone.name == name:
                return zone
        raise KeyError(name)
    d.addCallback(filter_results)
    return d



def _introducer_name_for_subscription(subscription_id, domain):
    return Name(u"{subscription_id}.introducer.{domain}".format(
        subscription_id=b32encode(subscription_id).lower().strip(u"="),
        domain=domain,
    ))



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


# def converge(subscriptions, k8s, aws):
#     return serially([
#         # Create and destroy deployments as necessary.  Use the
#         # subscription manager to find out what subscriptions are
#         # active and use look at the Kubernetes configuration to find
#         # out what subscription-derived deployments exist.
#         lambda: converge_deployments(subscriptions, k8s),

#         # Converge the rest of the system based on the result of that.
#         make_fan_out([
#             # Update the Kubernetes service so that it exposes the
#             # subscription-derived Deployments that now exist.
#             lambda deployments: converge_service(deployments, k8s),

#             # If there were changes, update the Route53 configuration.
#             lambda deployments: converge_route53(deployments, aws),
#         ]),
#     ])


# def converge_deployments(subscriptions, k8s):
#     active_subscriptions = subscriptions.list().addCallback(set)
#     configured_deployments = get_subscription_service(k8s)

#     d = DeferredList([active_subscriptions, configured_deployments])
#     d.addCallback()
#     d.addCallback(enact_configuration, k8s)
#     return d




# def converge_logic(desired, service):
#     # converge_deployment
#     # converge_configmap
#     # converge_route53
#     return converge_service(desired, service)

def get_ports(service):
    return service["spec"]["ports"]

def get_configured_subscriptions(service):
    # Every pair of i-... s-... ports is a configured subscription.

    def names(ports):
        return (port["name"] for port in ports)
    port_names = {
        name
        for name
        in names(get_ports(service))
        if name.startswith("i-") or name.startswith("s-")
    }

    def ids(names):
        return (name[2:] for name in names)
    subscriptions = {
        sid
        for sid
        in ids(port_names)
        if "i-" + sid in port_names and "s-" + sid in port_names
    }
    return subscriptions


def converge_service(desired, service):
    actual = get_configured_subscriptions(service)
    changes = compute_changes(desired, actual)
    # XXX Cannot update configuration without retrieving more state.
    new_service = update_configuration(changes, service)
    return new_service


@attr.s(frozen=True)
class Delete(object):
    subscription = attr.ib()

    def enact(self, service):
        return remove_subscription_from_service(service, self.subscription)

@attr.s(frozen=True)
class Create(object):
    subscription = attr.ib()

    def enact(self, service):
        return add_subscription_to_service(service, self.subscription)


def compute_changes(desired, actual):
    extra = actual - desired
    missing = desired - actual

    return map(Create, extra) + map(Delete, missing)

def update_configuration(changes, service):
    for change in changes:
        service = change.enact(service)
    return service

# def enact_configuration(service, k8s):
#     # XXX No such API
#     k8s.replace(service)

# @inlineCallbacks
# def serially(operations):
#     """
#     Call each of the given functions, one after the other.

#     The first function is called with no arguments.  Subsequent
#     functions are called with the result of the previous function.

#     If a function returns a Deferred, the next function is not called
#     until the Deferred fires.  Then it is called with the result of
#     the Deferred.

#     If a function raises an exception, no further functions are called
#     and the Deferred returned by this function fires with a Failure.

#     If all functions are executed without exception, the Deferred
#     returned by this function fires with the result of the last
#     function.
#     """
#     operations_iterator = iter(operations)
#     try:
#         first = next(operations_iterator)
#     except StopIteration:
#         return
#     result = yield first()
#     for op in operations_iterator:
#         result = yield op(result)
#     yield result


# def make_fan_out(operations):
#     """
#     Convert an iterable of functions to a single function which calls
#     each function.
#     """
#     def fan_out(result):
#         return DeferredList(list(
#             maybeDeferred(op, result)
#             for op
#             in operations
#         ))
#     return fan_out
