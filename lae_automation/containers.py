from base64 import b32encode
from json import dumps

from pyrsistent import freeze

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
			u"image": u"127.0.0.1:30000/leastauthority/tahoe-introducer:signup",
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
			u"image": u"127.0.0.1:30000/leastauthority/tahoe-storage:signup",
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



# Length is a factor for service names. :(
# Keep introducer and storage port names short.
def introducer_port_name(subscription_id):
    return u"i-" + subscription_id.replace(u"_", u"-")


def storage_port_name(subscription_id):
    return u"s-" + subscription_id.replace(u"_", u"-")


def create_deployment(deploy_config, details):
    name = deployment_name(details.subscription_id)
    configmap = configmap_name(details.subscription_id)
    metadata = subscription_metadata(details)

    # Length is a factor for these fields. :(
    intro_name = introducer_port_name(details.subscription_id)
    storage_name = storage_port_name(details.subscription_id)

    return DEPLOYMENT_TEMPLATE.transform(
        [u"metadata", u"namespace"], deploy_config.kubernetes_namespace,
        # Some other metadata to make inspecting this stuff a little easier.
        *metadata
    ).transform(
        # Point both configuration volumes at this subscriptions configmap.
        ["spec", "template", "spec", "volumes", 0, "configMap", "name"], configmap,
        ["spec", "template", "spec", "volumes", 1, "configMap", "name"], configmap,

        # Assign it service names and a port numbers.
        ["spec", "template", "spec", "containers", 0, "ports", 0],
        # XXX Don't really need unique ports here
        v1.ContainerPort(name=intro_name, containerPort=details.introducer_port_number),

        ["spec", "template", "spec", "containers", 1, "ports", 0],
        # XXX Or here
        v1.ContainerPort(name=storage_name, containerPort=details.storage_port_number),

        # And assign it a unique identifier the deployment can use to
        # refer to it.
        ["metadata", "name"], name
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



def service_ports(details):
    intro = v1.ServicePort(
        name=introducer_port_name(details.subscription_id),
        port=details.introducer_port_number,
        targetPort=introducer_port_name(details.subscription_id),
        protocol=u"TCP",
    )
    storage = v1.ServicePort(
        name=storage_port_name(details.subscription_id),
        port=details.storage_port_number,
        targetPort=storage_port_name(details.subscription_id),
        protocol=u"TCP",
    )
    return [intro, storage]



def add_subscription_to_service(old_service, details):
    return old_service.transform(
        ["spec", "ports"], extender(service_ports(details)),
        # Simplifies testing, probably helps in other areas.
        ["spec", "ports"], lambda v: freeze(sorted(v, key=lambda m: m.port)),
    )



def remove_subscription_from_service(old_service, subscription_id):
    ports = {
        introducer_port_name(subscription_id),
        storage_port_name(subscription_id),
    }
    return old_service.transform(
        # Where are my value-based transforms!
        ["spec", "ports"],
        filter(lambda p: p.name not in ports, old_service.spec.ports),
    )
