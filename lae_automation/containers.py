from json import dumps

from pyrsistent import freeze

from .server import new_tahoe_configuration, marshal_tahoe_configuration

def provision_subscription(reactor, deploy_config, details):
    """
    Create the resources necessary to provide service for a new
    subscription.
    """
    kube = KubeClient()
    old_service = get_service(kube, deploy_config)
    intro_number, storage_number = assign_ports(old_service, 2)
    
    configmap = create_configuration(deploy_config, details)
    deployment = create_deployment(deploy_config, intro_number, storage_number)
    new_service = add_subscription_to_service(
        deploy_config, details, old_service, intro_number, storage_number
    )
    dns = create_dns(deploy_config)

    create(kube, configmap, deployment, new_service, dns)
    
    record_serverinfo(reactor, deploy_config)
    send_confirmation(reactor, deploy_config)


def create():
    pass

def record_serverinfo():
    pass

def send_confirmation():
    pass


def get_service(kube, deploy_config):
    [service] = pykube.Service.objects(kube).filter(name=deploy_config.private_host)
    return freeze(service)


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
        ["metadata", "labels", "email"], details.email,
        ["metadata", "labels", "customer"], details.customer_id,
        ["metadata", "labels", "subscription"], details.subscription_id,
        ["metadata", "labels", "plan"], details.product_id,
    ]


def configmap_name(subscription_id):
    return "customer-config-" + subscription_id

def configmap_public_host(subscription_id, domain):
    return "{}.{}".format(subscription_id, domain)

def create_configuration(reactor, deploy_config, details, kube):
    """
    Create the Kubernetes configuration resource necessary to provide
    service to the given subscription.

    @param kube: The Kubernetes client to use to create the configuration.
    """
    name = configmap_name(details.subscription_id)
    metadata = subscription_metadata(details)
    public_host = configmap_public_host(details.subscription_id, deploy_config.domain)
    private_host = deploy_config.private_host

    if deploy_config.oldsecrets is None:
        configuration = new_tahoe_configuration(
            deploy_config=deploy_config,
            publichost=public_host,
            privatehost=private_host,
        )
    else:
        configuration = marshal_tahoe_configuration(
            introducer_pem=deploy_config.oldsecrets["introducer_node_pem"],
            storage_pem=deploy_config.oldsecrets["storageserver_node_pem"],
            storage_privkey=deploy_config.oldsecrets["server_node_privkey"],
            bucket_name=deploy_config.bucketname,
            publichost=public_host,
            privatehost=private_host,
            introducer_furl=deploy_config.oldsecrets["internal_introducer_furl"],
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


def create_deployment(deploy_config, details, intro_number, storage_number):
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
        ["spec", "template", "spec", "containers", 0, "ports", 0, "name"], intro_number,

        ["spec", "template", "spec", "containers", 1, "ports", 0, "name"], storage_name,
        ["spec", "template", "spec", "containers", 1, "ports", 0, "name"], storage_number,

        # And assign it a unique identifier the deployment can use to
        # refer to it.
        ["metadata", "name"], name
    )

SERVICE_TEMPLATE = freeze({
    "kind": "Service",
    "apiVersion": "v1",
    "metadata": {
	"name": MISSING,
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

def extender(values):
    def f(pvector):
        return pvector.extend(values)
    return f

def add_subscription_to_service(deploy_config, details, old_service, intro_number, storage_number):
    intro = {
        "name": introducer_port_name(details.subscription_id),
        "port": intro_number,
        "targetPort": introducer_port_name(details.subscription_id),
        "protocol":  "TCP",
    }
    storage = {
        "name": storage_port_name(details.subscription_id),
        "port": storage_number,
        "targetPort": storage_port_name(details.subscription_id),
        "protocol":  "TCP",
    }
    return old_service.transform(["spec", "ports"], extender([intro, storage]))


def create_dns():
    pass
