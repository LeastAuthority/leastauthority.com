from json import dumps

from pyrsistent import freeze, discard

from twisted.internet.defer import succeed

from .server import new_tahoe_configuration, marshal_tahoe_configuration

def provision_subscription(reactor, deploy_config, details, smclient):
    """
    Create the subscription state in the SubscriptionManager service.

    :param DeploymentConfiguration deploy_config:
    :param SubscriptionDetails details:
    """
    d = smclient.create(details.subscription_id, details)
    d.addCallback(lambda ignored: _wait_for_service(details.subscription_id))
    return d


def _wait_for_service(subscription_id):
    # XXX Poll Kubernetes state looking for matching resources.
    # XXX With a timeout and some error logging.
    return succeed(None)

MIN_PORT = 3000
MAX_PORT = 63000
def assign_ports(service, count):
    used = {port["port"] for port in service["spec"]["ports"]}
    total = set(range(MIN_PORT, MAX_PORT))
    avail = total - used
    i = iter(avail)
    return next(i), next(i)


class _NotSerializable(object):
    """
    A class which cannot be serialized.  Used as a placeholder for
    missing values in templates so that the template cannot be
    serialized if no value is provided.
    """

MISSING = _NotSerializable()


CONFIGMAP_TEMPLATE = freeze({
    "kind": "ConfigMap",
    "apiVersion": "v1",
    "metadata": {
	"name": MISSING,
	"labels": {
	    "provider": "LeastAuthority",
	    "app": "s4",
	    "component": "customer-tahoe-lafs"
	}
    },
    "data": {
	"storage.json": MISSING,
	"introducer.json": MISSING,
    }
})


def subscription_metadata(details):
    return [
        ["metadata", "labels", "email"], details.customer_email,
        ["metadata", "labels", "customer"], details.customer_id,
        ["metadata", "labels", "subscription"], details.subscription_id,
        ["metadata", "labels", "plan"], details.product_id,
    ]


def configmap_name(subscription_id):
    return "customer-config-" + subscription_id

def configmap_public_host(subscription_id, domain):
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

    if details.oldsecrets is None:
        configuration = new_tahoe_configuration(
            deploy_config=deploy_config,
            publichost=public_host,
            privatehost=private_host,
        )
    else:
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
        # Add some metadata to make inspecting this stuff a little
        # easier.
        *metadata
    ).transform(
        # Dump the actual Tahoe-LAFS configuration into it.
        ["data", "introducer.json"], dumps(configuration["introducer"]),
        ["data", "storage.json"], dumps(configuration["storage"]),

        # And assign it a unique identifier the deployment can use to
        # refer to it.
        ["metadata", "name"], name
    )

DEPLOYMENT_TEMPLATE = freeze({
    "kind": "Deployment",
    "apiVersion": "extensions/v1beta1",
    "metadata": {
	"name": MISSING,
    },
    "spec": {
	"replicas": 1,
	"template": {
	    "metadata": {
		"labels": {
		    "provider": "LeastAuthority",
		    "app": "s4",
		    "component": "customer-tahoe-lafs"
		}
	    },
	    "spec": {
		"volumes": [
		    {
			"name": "introducer-config-volume",
			"configMap": {
			    "name": MISSING,
			    "items": [
				{
				    "key": "introducer.json",
				    "path": "introducer.json"
				}
			    ]
			}
		    },
		    {
			"name": "storage-config-volume",
			"configMap": {
			    "name": MISSING,
			    "items": [
				{
				    "key": "storage.json",
				    "path": "storage.json"
				}
			    ]
			}
		    }
		],
		"containers": [
		    {
			"name": "introducer",
			"image": "127.0.0.1:30000/leastauthority/tahoe-introducer:signup",
			"volumeMounts": [
			    {
				"name": "introducer-config-volume",
				"mountPath": "/app/config"
			    }
			],
			"ports": [
			    {
				"name": MISSING,
				"containerPort": MISSING,
			    }
			]
		    },
		    {
			"name": "storageserver",
			"image": "127.0.0.1:30000/leastauthority/tahoe-storage:signup",
			"volumeMounts": [
			    {
				"name": "storage-config-volume",
				"mountPath": "/app/config"
			    }
			],
			"ports": [
			    {
				"name": MISSING,
				"containerPort": MISSING,
			    }
			]
		    }
		]
	    }
	}
    }
})

def deployment_name(subscription_id):
    return "customer-deployment-" + subscription_id


# Length is a factor for service names. :(
# Keep introducer and storage port names short.
def introducer_port_name(subscription_id):
    return "i-" + subscription_id.replace("_", "-")


def storage_port_name(subscription_id):
    return "s-" + subscription_id.replace("_", "-")


def create_deployment(deploy_config, details):
    name = deployment_name(details.subscription_id)
    configmap = configmap_name(details.subscription_id)
    metadata = subscription_metadata(details)

    # Length is a factor for these fields. :(
    intro_name = introducer_port_name(details.subscription_id)
    storage_name = storage_port_name(details.subscription_id)

    return DEPLOYMENT_TEMPLATE.transform(
        # Add some metadata to make inspecting this stuff a little
        # easier.
        *metadata
    ).transform(
        # Point both configuration volumes at this subscriptions configmap.
        ["spec", "template", "spec", "volumes", 0, "configMap", "name"], configmap,
        ["spec", "template", "spec", "volumes", 1, "configMap", "name"], configmap,

        # Assign it service names and a port numbers.
        ["spec", "template", "spec", "containers", 0, "ports", 0, "name"], intro_name,
        ["spec", "template", "spec", "containers", 0, "ports", 0, "containerPort"], intro_name,

        ["spec", "template", "spec", "containers", 1, "ports", 0, "name"], storage_name,
        ["spec", "template", "spec", "containers", 1, "ports", 0, "containerPort"], storage_name,

        # And assign it a unique identifier the deployment can use to
        # refer to it.
        ["metadata", "name"], name
    )

EMPTY_SERVICE = freeze({
    "kind": "Service",
    "apiVersion": "v1",
    "metadata": {
        # XXX ???
	"name": "s4-customer-grids",
	"labels": {
	    "provider": "LeastAuthority"
	}
    },
    "spec": {
	"type": "LoadBalancer",
	"selector": {
	    "provider": "LeastAuthority",
	    "app": "s4",
	    "component": "customer-tahoe-lafs"
	},
	"ports": [
	]
    }
})

def new_service():
    return EMPTY_SERVICE

def extender(values):
    def f(pvector):
        return pvector.extend(values)
    return f

def service_ports(details):
    intro = {
        "name": introducer_port_name(details.subscription_id),
        "port": details.introducer_port_number,
        "targetPort": introducer_port_name(details.subscription_id),
        "protocol":  "TCP",
    }
    storage = {
        "name": storage_port_name(details.subscription_id),
        "port": details.storage_port_number,
        "targetPort": storage_port_name(details.subscription_id),
        "protocol":  "TCP",
    }
    return [intro, storage]


def add_subscription_to_service(old_service, details):
    return old_service.transform(
        ["spec", "ports"], extender(service_ports(details)),
        # Simplifies testing, probably helps in other areas.
        ["spec", "ports"], lambda v: freeze(sorted(v, key=lambda m: m["port"])),
    )

def remove_subscription_from_service(old_service, details):
    ports = {
        introducer_port_name(details.subscription_id),
        storage_port_name(details.subscription_id),
    }
    return old_service.transform(
        ["spec", "ports", lambda p: p["name"] in ports],
        discard,
    )

def create_dns():
    pass
