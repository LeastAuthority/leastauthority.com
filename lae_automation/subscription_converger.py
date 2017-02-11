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

import attr

from eliot import Message, start_action
from eliot.twisted import DeferredContext

from twisted.internet.defer import Deferred, inlineCallbacks, returnValue, maybeDeferred
from twisted.application.internet import TimerService
from twisted.python.usage import Options as _Options
from twisted.python.url import URL
from twisted.web.client import Agent

from txaws.route53.model import Name, CNAME, delete_rrset, create_rrset

from .signup import DeploymentConfiguration
from .subscription_manager import Client as SMClient
from .containers import (
    configmap_name, deployment_name,
    create_configuration, create_deployment,
    new_service,
    add_subscription_to_service, remove_subscription_from_service
)
from .kubeclient import KubeClient, LabelSelector


from txkube import v1, network_kubernetes, authenticate_with_serviceaccount

class Options(_Options):
    optParameters = [
        ("endpoint", "e", None, "The root URL of the subscription manager service."),
    ]

def makeService(options):
    from twisted.internet import reactor
    agent = Agent(reactor)
    subscription_client = SMClient(endpoint=options["endpoint"], agent=agent)

    k8s_client = network_kubernetes(
        base_url=URL.fromText(u"https://kubernetes/"),
        agent=authenticate_with_serviceaccount(reactor),
    )
    k8s = KubeClient(k8s=k8s_client)

    config = DeploymentConfiguration()

    return TimerService(
        1.0,
        divert_errors_to_log(converge), config, subscription_client, k8s,
    )

def divert_errors_to_log(f):
    def g(*a, **kw):
        action = start_action(u"subscription_converger:" + f.__name__)
        with action.context():
            d = DeferredContext(maybeDeferred(f, *a, **kw))
            d.addActionFinish()
            # The failure was logged by the above.  Now squash it.
            d.addErrback(lambda err: None)
            return d.result
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


def get_customer_grid_service(k8s):
    action = start_action(action_type=u"load-services")
    with action.context():
        d = DeferredContext(k8s.get_services(LabelSelector(dict(
            provider="LeastAuthority",
            app="s4",
            component="customer-tahoe-lafs"
        ))))
        def got_services(services):
            services = list(services)
            action.add_success_fields(service_count=len(services))
            if services:
                return services[0]
            return None
        d.addCallback(got_services)
        return d.addActionFinish()


def get_customer_grid_deployments(k8s):
    action = start_action(action_type=u"load-deployments")
    with action.context():
        d = DeferredContext(k8s.get_deployments(LabelSelector(dict(
            provider="LeastAuthority",
            app="s4",
            component="customer-tahoe-lafs"
        ))))
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


@with_action(action_type=u"converge")
@inlineCallbacks
def converge(config, subscriptions, k8s, aws):
    # Create and destroy deployments as necessary.  Use the
    # subscription manager to find out what subscriptions are active
    # and use look at the Kubernetes configuration to find out what
    # subscription-derived deployments exist.  Also detect port
    # mis-configurations and correct them.
    active_subscriptions = yield get_active_subscriptions(subscriptions)
    configured_deployments = yield get_customer_grid_deployments(k8s)
    configured_service = (yield get_customer_grid_service(k8s))
    create_service = (configured_service is None)
    if create_service:
        configured_service = new_service()

    to_create = set(active_subscriptions.itervalues())
    to_delete = set()

    for deployment in configured_deployments:
        subscription_id = deployment["metadata"]["labels"]["subscription"]
        try:
            subscription = active_subscriptions[subscription_id]
        except KeyError:
            to_delete.add(subscription_id)
            continue

        to_create.remove(subscription)

        introducer_port = (
            deployment["spec"]["template"]["spec"]["containers"][0]["ports"][0]["containerPort"]
        )
        storage_port = (
            deployment["spec"]["template"]["spec"]["containers"][1]["ports"][0]["containerPort"]
        )
        if introducer_port != subscription.introducer_port_number:
            Message.log(modify_deployment=subscription_id, reason=u"introducer port mismatch")
            to_delete.add(subscription_id)
            to_create.add(subscription)
        elif storage_port != subscription.storage_port_number:
            Message.log(modify_deployment=subscription_id, reason=u"storage port mismatch")
            to_delete.add(subscription_id)
            to_create.add(subscription)

    configmaps = list(
        create_configuration(config, details)
        for details
        in to_create
    )
    deployments = list(
        create_deployment(config, details)
        for details
        in to_create
    )
    service = apply_service_changes(configured_service, to_delete, to_create)
    assert isinstance(service, v1.Service)

    route53 = aws.get_route53_client()
    try:
        zone = yield get_hosted_zone_by_name(route53, config.domain)
    except KeyError:
        raise ValueError("Configured domain {!r} does not exist in Route53.".format(config.domain))

    a = start_action(
        action_type=u"enacting",
        to_delete=list(to_delete),
        to_create=list(s.subscription_id for s in to_create),
    )
    # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # the objects have no namespace so they can't be created
    # also this function is now (I)/O -> logic -> I/(O)
    # so probably could be refactored now
    with a:
        yield delete_route53_rrsets(route53, zone, to_delete)
        for subscription_id in to_delete:
            yield k8s.destroy("deployment", deployment_name(subscription_id))
            yield k8s.destroy("configmap", configmap_name(subscription_id))

        for configmap in configmaps:
            yield k8s.create(configmap)
        for deployment in deployments:
            yield k8s.create(deployment)
        if create_service:
            yield k8s.create(service)
        else:
            yield k8s.replace(service)
        yield create_route53_rrsets(route53, zone, to_create)


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
        subscription_id=subscription_id,
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

def delete_route53_rrsets(route53, zone, subscription_ids):
    a = start_action(action_type=u"delete-route53", subscription_ids=list(subscription_ids))
    with a.context():
        d = route53.change_resource_record_sets(zone.identifier, list(
            delete_rrset(
                name=_introducer_name_for_subscription(subscription_id, zone.name),
                type=u"CNAME",
                rrset=[_cname_for_subscription(zone.name)],
            )
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
            create_rrset(
                name=_introducer_name_for_subscription(subscription.subscription_id, zone.name),
                type=u"CNAME",
                rrset=[_cname_for_subscription(zone.name)],
            )
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
