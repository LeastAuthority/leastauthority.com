from itertools import count
from base64 import b32encode
from json import dumps

from pyrsistent import freeze, discard, ny

from .server import marshal_tahoe_configuration

from txkube import v1, v1beta1

_S4_METADATA = v1.ObjectMeta(
    labels={
        # Some labels that help us identify stuff belonging to
        # customer-specific pieces (deployments, configmaps, etc) of the
        # service.
	u"provider": u"LeastAuthority",
	u"app": u"s4",
	u"component": u"customer-tahoe-lafs",
        # And version this thing so we know how to interpret whatever else we
        # find in it.
        u"version": u"1",
    }
)



CONFIGMAP_TEMPLATE = v1.ConfigMap(
    metadata=_S4_METADATA,
)



def subscription_metadata(details):
    return [
        # Some information about the customer/subscription to be attached to
        # objects that exist specifically for that customer/subscription.
        [u"metadata", u"annotations", u"email"], details.customer_email,
        [u"metadata", u"annotations", u"customer"], details.customer_id,
        [u"metadata", u"annotations", u"subscription"], details.subscription_id,
        [u"metadata", u"annotations", u"plan"], details.product_id,
    ]



def configmap_name(subscription_id):
    return u"customer-config-" + _sanitize(subscription_id)



def configmap_public_host(subscription_id, domain):
    # XXX Fishy
    return "{}.{}".format(subscription_id, domain)



def create_configuration(deploy_config, details):
    """
    Create the Kubernetes configuration resource necessary to provide
    service to the given subscription.
    """
    name = configmap_name(details.subscription_id)
    metadata = subscription_metadata(details)
    public_host = configmap_public_host(details.subscription_id, deploy_config.domain)
    private_host = deploy_config.private_host

    configuration = marshal_tahoe_configuration(
        introducer_pem=details.introducer_node_pem,
        storage_pem=details.server_node_pem,
        storage_privkey=details.oldsecrets["server_node_privkey"],
        bucket_name=details.bucketname,
        publichost=public_host,
        privatehost=private_host,
        introducer_furl=details.oldsecrets["internal_introducer_furl"],
        s3_access_key_id=deploy_config.s3_access_key_id,
        s3_secret_key=deploy_config.s3_secret_key,
        log_gatherer_furl=deploy_config.log_gatherer_furl,
        stats_gatherer_furl=deploy_config.stats_gatherer_furl,
    )

    return CONFIGMAP_TEMPLATE.transform(
        [u"metadata", u"namespace"], deploy_config.kubernetes_namespace,
        # Assign it a unique identifier the deployment can use to refer to it.
        [u"metadata", u"name"], name,
        # Some other metadata to make inspecting this stuff a little easier.
        *metadata
    ).transform(
        # Dump the actual Tahoe-LAFS configuration into it.
        [u"data", u"introducer.json"], dumps(configuration["introducer"]).decode("ascii"),
        [u"data", u"storage.json"], dumps(configuration["storage"]).decode("ascii"),
    )



DEPLOYMENT_TEMPLATE = v1beta1.Deployment(
    metadata=_S4_METADATA,
    status=None,
    spec={
	u"replicas": 1,
        # Don't keep an arbitrarily large amount of history around.
        u"revisionHistoryLimit": 2,
	u"template": {
	    u"metadata": _S4_METADATA,
	    u"spec": {
		u"volumes": [
		    {
			u"name": u"introducer-config-volume",
			u"configMap": {
			    u"items": [
				{
				    u"key": u"introducer.json",
				    u"path": u"introducer.json"
				}
			    ]
			}
		    },
		    {
			u"name": u"storage-config-volume",
			u"configMap": {
			    u"items": [
				{
				    u"key": u"storage.json",
				    u"path": u"storage.json"
				}
			    ]
			}
		    }
		],
		u"containers": [
		    {
			u"name": u"introducer",
			u"image": u"127.0.0.1:30000/leastauthority/tahoe-introducer:3dcc82f",
			u"volumeMounts": [
			    {
				u"name": u"introducer-config-volume",
				u"mountPath": u"/app/config"
			    }
			],
                        u"ports": [],
		    },
		    {
			u"name": u"storageserver",
			u"image": u"127.0.0.1:30000/leastauthority/tahoe-storage:3dcc82f",
			u"volumeMounts": [
			    {
				u"name": u"storage-config-volume",
				u"mountPath": u"/app/config"
			    }
			],
                        u"ports": [],
		    }
		]
	    }
	}
    }
)



def _sanitize(subscription_id):
    return b32encode(subscription_id).lower().strip(u"=")



def deployment_name(subscription_id):
    return u"customer-deployment-" + _sanitize(subscription_id)



def create_deployment(deploy_config, details):
    name = deployment_name(details.subscription_id)
    configmap = configmap_name(details.subscription_id)

    # We need to make this Deployment distinct from the similar
    # deployments that exist for all other subscriptions.
    subscription_annotation = b32encode(details.subscription_id).lower().strip(u"=")

    # The names don't really matter.  They need to be unique within the scope
    # of the Deployment, I think.  Hey look they are.
    intro_name = u"introducer"
    storage_name = u"storage"

    return DEPLOYMENT_TEMPLATE.transform(
        # Make sure it ends up in our namespace.
        [u"metadata", u"namespace"], deploy_config.kubernetes_namespace,

        # Assign it a unique identifier the deployment can use to refer to it.
        [u"metadata", u"name"], name,

        # Also make it easy to find by subscription.
        [u"metadata", u"labels", u"subscription"], subscription_annotation,

        # Make the pod for this subscription's deployment distinct from the
        # pods for all the other subscriptions' deployments.
        [u"spec", u"template", u"metadata", u"labels", u"subscription"],
        subscription_annotation,

        # Point both configuration volumes at this subscription's configmap.
        ["spec", "template", "spec", "volumes", ny, "configMap", "name"], configmap,

        # Assign it service names and a port numbers.
        ["spec", "template", "spec", "containers", 0, "ports", 0],
        # XXX Don't really need unique ports here
        v1.ContainerPort(name=intro_name, containerPort=details.introducer_port_number),
        ["spec", "template", "spec", "containers", 1, "ports", 0],
        # XXX Or here
        v1.ContainerPort(name=storage_name, containerPort=details.storage_port_number),

        # Some other metadata to make inspecting this stuff a little easier.
        *subscription_metadata(details)
    )

EMPTY_SERVICE = v1.Service(
    metadata=_S4_METADATA.set(u"name", u"s4-customer-grids"),
    spec={
	u"type": u"LoadBalancer",
	u"selector": _S4_METADATA.labels,
    }
)

def new_service(namespace):
    return EMPTY_SERVICE.transform(
        [u"metadata", u"namespace"], namespace,
    )


def extender(values):
    def f(pvector):
        return pvector.extend(values)
    return f



def distinct_port_name(service, kind, seed):
    """
    Compute name, not already used in ``service``, for a new subscription.
    """
    existing_names = set(port.name for port in service.spec.ports)
    for i in count():
        name = u"{}{}{}".format(i, kind, seed)[:15].lower().replace(u"_", u"-")
        if name not in existing_names:
            return name


def service_ports(old_service, details):
    # We need to be able to easily identify which subscription owns which
    # ports later on.  It probably _would_ be possible for the deletion
    # codepath to find these values from the subscription manager
    # database... And avoiding duplication might even be a really good thing.
    # However, some cases might require particular attention.  For example,
    # what if a subscription is deactivated and then another one is activated
    # and assigned the same ports, and _then_ the convergence loop runs.
    # Based solely on port numbers, will we delete the right ones?
    introducer_name = distinct_port_name(
        old_service, u"i", details.subscription_id,
    )
    storage_name = distinct_port_name(
        old_service, u"s", details.subscription_id,
    )
    intro = v1.ServicePort(
        name=introducer_name,
        port=details.introducer_port_number,
        targetPort=details.introducer_port_number,
        protocol=u"TCP",
    )
    storage = v1.ServicePort(
        name=storage_name,
        port=details.storage_port_number,
        targetPort=details.storage_port_number,
        protocol=u"TCP",
    )
    return [intro, storage]



def add_subscription_to_service(old_service, details):
    """
    Update a ``txkube.v1.Service`` to include details necessary to provide
    service to the given subscription.

    :param txkube.v1.Service old_service: An existing service object to
        update.

    :param SubscriptionDetails details: The details of the subscription to add
        to the service.

    :return txkube.v1.Service: A modified version of ``old_service`` that
        accounts for ``details``.
    """
    ports = service_ports(old_service, details)

    return old_service.transform(
        # Put the identifiers into the metadata area of the service.
        [u"metadata", u"annotations", u"leastauthority.com/subscription/" + details.subscription_id],
        u"v1 {} {}".format(ports[0].name, ports[1].name),

        # Each subscription has been allocated two public-facing port (one
        # embedded in the introducer furl for that subscription, one reachable
        # with information retrieved from the introducer).  The service needs
        # to be updated to route those port into the right pods.  For now, the
        # internal and external ports match.  That means we just add two
        # entries to the service's ports list with port / targetPort set to
        # the subscriptions values.
        [u"spec", u"ports"], extender(ports),

        # Also keep the entries in port number-sorted order.  This simplifies
        # testing and maybe probably helps in other areas (by reducing the
        # possible number of states the service can be in, I guess).
        [u"spec", u"ports"], lambda v: freeze(sorted(v, key=lambda m: m.port)),

    )



def remove_subscription_from_service(old_service, subscription_id):
    port_names = old_service.metadata.annotations[subscription_id]
    version = port_names.split()[0]
    if version != u"v1":
        raise ValueError("Cannot interpret port name metadata version " + version)
    _, introducer_name, storage_name =  port_names.split()

    ports = {introducer_name, storage_name}
    return old_service.transform(
        # Take the annotation out of the service.
        [u"metadata", u"annotations", subscription_id],
        discard,

        # Take the port out of the service.
        [u"spec", u"ports"],
        # Where are my value-based transforms!
        filter(lambda p: p.name not in ports, old_service.spec.ports),
    )
