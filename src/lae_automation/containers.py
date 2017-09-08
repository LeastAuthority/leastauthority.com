# Copyright Least Authority Enterprises.
# See LICENSE for details.

from base64 import b32encode, b32decode
from json import dumps
from hashlib import sha256

from eliot import Message

from pyrsistent import ny

from twisted.python.filepath import FilePath

from .server import marshal_tahoe_configuration

CONTAINERIZED_SUBSCRIPTION_VERSION = u"2"

# This metadata is associated with everything that is part of S4 which is
# associated with per-customer resources.  For example, Deployments,
# ConfigMaps, ReplicaSets, Pods.
CUSTOMER_METADATA_LABELS = {
    # Some labels that help us identify stuff belonging to customer-specific
    # pieces (deployments, configmaps, etc) of the service.
    u"provider": u"LeastAuthority",
    u"app": u"s4",
    u"component": u"customer-tahoe-lafs",
    # And version this thing so we know how to interpret whatever else we find
    # in it.
    u"version": CONTAINERIZED_SUBSCRIPTION_VERSION,
}



def _s4_customer_metadata(model):
    return model.v1.ObjectMeta(labels=CUSTOMER_METADATA_LABELS)



def _s4_infrastructure_metadata(model):
    return _s4_customer_metadata(model).transform(
        [u"labels", u"component"], u"Infrastructure",
    )



def subscription_metadata(details):
    return [
        # Some information about the customer/subscription to be attached to
        # objects that exist specifically for that customer/subscription.
        [u"metadata", u"annotations", u"email"], details.customer_email,
        [u"metadata", u"annotations", u"customer"], details.customer_id,
        [u"metadata", u"annotations", u"subscription"], details.subscription_id,
        [u"metadata", u"annotations", u"plan"], details.product_id,
        [u"metadata", u"annotations", u"leastauthority.com/introducer-tub-id"],
        details.introducer_tub_id,
        [u"metadata", u"annotations", u"leastauthority.com/storage-tub-id"],
        details.storage_tub_id,
        [u"metadata", u"annotations", u"leastauthority.com/introducer-port-number"],
        u"{}".format(details.introducer_port_number),
        [u"metadata", u"annotations", u"leastauthority.com/storage-port-number"],
        u"{}".format(details.storage_port_number),
    ]



def configmap_name(subscription_id):
    return u"customer-config-" + _sanitize(subscription_id)



def configmap_public_host(subscription_id, domain):
    return u"{subscription_id}.introducer.{domain}".format(
        subscription_id=b32encode(subscription_id).lower().strip(u"="),
        domain=domain,
    )



def create_configuration(deploy_config, details, model):
    """
    Create the Kubernetes configuration resource necessary to provide
    service to the given subscription.
    """
    configmap_template = model.v1.ConfigMap(
        metadata=_s4_customer_metadata(model),
    )

    name = configmap_name(details.subscription_id)
    metadata = subscription_metadata(details)
    public_host = configmap_public_host(details.subscription_id, deploy_config.domain)
    private_host = deploy_config.private_host

    Message.log(
        event=u"convergence-service:key-notification",
        key_id=deploy_config.s3_access_key_id,
        secret_key_hash=sha256(deploy_config.s3_secret_key).hexdigest().decode("ascii"),
    )

    configuration = marshal_tahoe_configuration(
        introducer_pem=details.introducer_node_pem,
        storage_pem=details.server_node_pem,
        storage_privkey=details.oldsecrets["server_node_privkey"],
        introducer_port=details.introducer_port_number,
        storageserver_port=details.storage_port_number,
        bucket_name=details.bucketname,
        key_prefix=details.key_prefix,
        publichost=public_host,
        privatehost=private_host,
        introducer_furl=details.external_introducer_furl,
        # Note we get the keys from the deployment configuration passed to us.
        # oldsecrets has this junk in it, too, but it's unreliable and we want
        # to get rid of that storage location for the information.  Our
        # deployment configuration is more reliable anyway and lets us change
        # values more easily (in one place instead of N).
        s3_access_key_id=deploy_config.s3_access_key_id,
        s3_secret_key=deploy_config.s3_secret_key,
        log_gatherer_furl=deploy_config.log_gatherer_furl,
        stats_gatherer_furl=deploy_config.stats_gatherer_furl,
    )

    return configmap_template.transform(
        [u"metadata", u"namespace"], deploy_config.kubernetes_namespace,
        # Assign it a unique identifier the deployment can use to refer to it.
        [u"metadata", u"name"], name,
        # Some other metadata to make inspecting this stuff a little easier.
        *metadata
    ).transform(
        # Dump the actual Tahoe-LAFS configuration into it.
        [u"data", u"introducer.json"],
        dumps({"introducer": configuration["introducer"]}).decode("ascii"),

        [u"data", u"storage.json"],
        dumps({"storage": configuration["storage"]}).decode("ascii"),
    )


def _deployment_template(model):
    introducer_liveness_sidecar = create_liveness_container(
        model=model,
        port=8080,
        volumeName=u"introducer-config-volume",
        configItem=u"introducer.json",
    )
    storageserver_liveness_sidecar = create_liveness_container(
        model=model,
        port=8081,
        volumeName=u"storage-config-volume",
        configItem=u"storage.json",
    )
    return model.v1beta1.Deployment(
        metadata=_s4_customer_metadata(model),
        status=None,
        spec={
	    u"replicas": 1,
            u"selector": {
                u"matchExpressions": None,
                u"matchLabels": None,
            },
            # Don't keep an arbitrarily large amount of history around.
            u"revisionHistoryLimit": 2,
	    u"template": {
	        u"metadata": _s4_customer_metadata(model),
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
                        introducer_liveness_sidecar,
                        storageserver_liveness_sidecar,
		        {
                            # The image is filled in at instantiation time.
			    u"name": u"introducer",
			    u"volumeMounts": [
			        {
				    u"name": u"introducer-config-volume",
				    u"mountPath": u"/app/config"
			        }
			    ],
                            u"ports": [],
                            u"livenessProbe": create_liveness_probe(model, introducer_liveness_sidecar),
                            # https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/
                            u"resources": {
                                u"requests": {
                                    # The introducer has a very simple job to do.
                                    # The client doesn't need to interact with it
                                    # very often, either.  This overrides the
                                    # default of 100m which is much too much.
                                    u"cpu": u"5m",

                                    # Observed virtual size of the introducer
                                    # around 206MiB.  Observed resident size of
                                    # the introducer around 54MiB.  Normally I
                                    # don't think the working set size of the
                                    # introducer should change much - it does very
                                    # little work.
                                    u"memory": u"64Mi",
                                },
                            },
		        },
		        {
                            # The image is filled in at instantiation time.
			    u"name": u"storageserver",
			    u"volumeMounts": [
			        {
				    u"name": u"storage-config-volume",
				    u"mountPath": u"/app/config"
			        }
			    ],
                            u"ports": [],
                            u"livenessProbe": create_liveness_probe(model, storageserver_liveness_sidecar),
                            # https://kubernetes.io/docs/concepts/configuration/manage-compute-resources-container/
                            u"resources": {
                                u"requests": {
                                    # The storage server needs to shuffle bytes
                                    # from the backend to the client.  This is a
                                    # fairly inexpensive operation.  Give this
                                    # container more CPU than the introducer but
                                    # still not much.  Less than the default of
                                    # 100m (which would limit us to 10 storage
                                    # servers per CPU).
                                    u"cpu": u"15m",

                                    # Observed virtual size of the introducer
                                    # around 299MiB.  Observed resident size of
                                    # the introducer around 64MiB.  I would expect
                                    # the working set size of the storage server
                                    # to vary more than that of the introducer.
                                    # It handles buffering of more data flowing
                                    # between clients and S3.
                                    u"memory": u"128Mi",
                                },
                            },
		        },
		    ]
	        }
	    }
        }
    )



def create_liveness_container(model, port, volumeName, configItem):
    # This extra container in the customer deployment pods watches for
    # configuration file changes and signals non-liveness when they occur.
    # This provokes Kubernetes into restarting the container on which the
    # liveness probe is defined - allowing it to pick up the new
    # configuration.  If Tahoe-LAFS could re-read its configuration file we
    # might be able to avoid this whole thing.
    mountpoint = FilePath(u"/config")
    return model.v1.Container(**{
        u"name": u"config-liveness-sidecar-{}".format(port),
        u"imagePullPolicy": u"Always",
        u"image": u"leastauthority/config-file-liveness-server",
        u"command": [u"/sbin/tini", u"--"],
        u"args": [
            u"/usr/local/bin/config-file-liveness-server-exe",
            u"{}".format(port),
            mountpoint.child(configItem).path,
        ],
        u"volumeMounts": [{
            u"name": volumeName,
            u"mountPath": mountpoint.path,
        }],
        u"ports": [{
            u"containerPort": port,
        }],
        u"resources": {
            u"requests": {
                u"cpu": u"1m",
                u"memory": u"32Ki",
            },
        },
    })



def create_liveness_probe(model, container):
    # https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-probes/
    return model.v1.Probe(**{
        u"httpGet": {
            u"path": u"/",
            u"port": container.ports[0].containerPort,
        },
        u"failureThreshold": 1,
        u"successThreshold": 1,
        u"initialDelaySeconds": 5,
        u"periodSeconds": 600,
        u"timeoutSeconds": 5,
    })



def _sanitize(subscription_id):
    return b32encode(subscription_id).lower().strip(u"=")



def deployment_name(subscription_id):
    return u"customer-deployment-" + _sanitize(subscription_id)



def _named_pred(name):
    def predicate(key, value):
        return value.name == name
    return predicate



def profile_deployment(model, deployment):
    def transformer_kind(op):
        def make_transformer(element):
            def transform(target):
                return getattr(target, op)(element)
            return transform
        return make_transformer

    append = transformer_kind("append")

    pod_spec = [u"spec", u"template", u"spec"]
    storageserver = pod_spec + [u"containers", _named_pred(u"storageserver")]

    # profiler = u"cProfileCPU"
    profiler = u"TheseusHook"

    return deployment.transform(
        pod_spec + [u"volumes"],
        append(model.v1.Volume(
            name=u"tahoe-profiles",
            persistentVolumeClaim={u"claimName": u"tahoe-profiles"},
        )),

        storageserver + [u"args"],
        [
            u"/bin/sh", u"-c",
            # Largely copy/paste from Dockerfile.tahoe-storage but with the
            # addition of the profiling command.
            u"""
            /app/env/bin/python /app/configure-tahoe /var/run/storageserver < /app/config/storage.json
                && exec /app/env/bin/python -m {profiler} -o /profiles/tahoe-$(date +%s).stats \
                            /app/env/bin/tahoe run /var/run/storageserver
            """.replace(u"\n", u" ").format(profiler=profiler),
        ],

        storageserver + [u"volumeMounts"],
        append(model.v1.VolumeMount(mountPath=u"/profiles", name=u"tahoe-profiles")),
    )



def create_deployment(deploy_config, details, model):
    name = deployment_name(details.subscription_id)
    configmap = configmap_name(details.subscription_id)

    # We need to make this Deployment distinct from the similar
    # deployments that exist for all other subscriptions.
    subscription_label = b32encode(details.subscription_id).lower().strip(u"=")

    # The names don't really matter.  They need to be unique within the scope
    # of the Deployment, I think.  Hey look they are.
    intro_name = u"introducer"
    storage_name = u"storage"

    # Compute a small amount of pseudo-random jitter to add to the liveness
    # probe interval for the given container.  This serves to de-synchronize
    # liveness probes across pods for different subscriptions to avoid having
    # them all restarted in one thundering herd.
    jitter = hash(details.subscription_id) % 1200

    deployment = _deployment_template(model).transform(
        # Make sure it ends up in our namespace.
        [u"metadata", u"namespace"], deploy_config.kubernetes_namespace,

        # Assign it a unique identifier the deployment can use to refer to it.
        [u"metadata", u"name"], name,

        # Also make it easy to find by subscription.
        [u"metadata", u"labels", u"subscription"], subscription_label,

        # Make the pod for this subscription's deployment distinct from the
        # pods for all the other subscriptions' deployments.
        [u"spec", u"template", u"metadata", u"labels", u"subscription"],
        subscription_label,

        # Point both configuration volumes at this subscription's configmap.
        ["spec", "template", "spec", "volumes", ny, "configMap", "name"], configmap,

        # The deployment configuration tells us what images we're supposed to
        # be using at the moment.
        [u"spec", u"template", u"spec", u"containers", _named_pred(u"introducer"), u"image"],
        deploy_config.introducer_image,

        # The deployment configuration tells us what images we're supposed to
        # be using at the moment.
        [u"spec", u"template", u"spec", u"containers", _named_pred("storageserver"), u"image"],
        deploy_config.storageserver_image,

        # Assign it service names and a port numbers.
        # Though the only hard constraint Kubernetes implies on the port is
        # that it be unique within the pod, since we want to expose these via
        # a v1.Service, we need to make them unique across all pods the
        # service is going to select.  That's all pods for customer grids.
        ["spec", "template", "spec", "containers", _named_pred(u"introducer"), "ports", 0],
        model.v1.ContainerPort(name=intro_name, containerPort=details.introducer_port_number),

        ["spec", "template", "spec", "containers", _named_pred(u"storageserver"), "ports", 0],
        model.v1.ContainerPort(name=storage_name, containerPort=details.storage_port_number),

        # Add some jitter to the liveness probe intervals to avoid a
        # thundering herd on configuration updates and such.
        ["spec", "template", "spec", "containers", _named_pred(u"introducer"), "livenessProbe", "periodSeconds"],
        lambda seconds: seconds + jitter,

        ["spec", "template", "spec", "containers", _named_pred(u"storageserver"), "livenessProbe", "periodSeconds"],
        lambda seconds: seconds + jitter,

        # Some other metadata to make inspecting this stuff a little easier.
        # First added to the pod template...
        [u"spec", u"template"],
        lambda template: template.transform(*subscription_metadata(details)),

        # ... and then to the deployment spec.
        *subscription_metadata(details)
    )
    deployment = deployment.transform(
        # Be explicit about how we select replicasets and pods belonging to
        # this deployment.  This probably lines up with the default behavior
        # if a selector is omitted.  It's nice to be explicit here since it
        # reduces the amount of stuff about the Deployment that changes after
        # it's submitted to Kubernetes for creation.
        [u"spec", u"selector", u"matchLabels"], deployment.metadata.labels,
    )

    # XXXX HAHA
    if u"+profile@" in details.customer_email:
        deployment = profile_deployment(model, deployment)

    return deployment



# This service exposes all the customer introducers and storage servers to the public internet.
# Some background on performance of many-ports Services:
# http://blog.kubernetes.io/2016/09/high-performance-network-policies-kubernetes.html
S4_CUSTOMER_GRID_NAME = u"s4-customer-grids"

def _customer_grid_service(model):
    return model.v1.Service(
        metadata=_s4_customer_metadata(model).set(
            u"name", S4_CUSTOMER_GRID_NAME,
        ).set(
            u"annotations", {
                # The default idle timeout in both Kubernetes (1.5.x) and AWS
                # appears to be 60 seconds.  That's annoyingly low for Tahoe-LAFS
                # connections.  Let's raise it a bit.
                #
                # https://github.com/LeastAuthority/LeastAuthoritarians/issues/191
                u"service.beta.kubernetes.io/aws-load-balancer-connection-idle-timeout": u"3600",
            },
        ),
        spec={
	    u"type": u"LoadBalancer",
            # We don't actually want to select the "customer" pods here.  Instead,
            # want the grid router pod.
	    u"selector": _s4_infrastructure_metadata(model).labels,
            u"ports": [
                {u"name": u"introducer", u"protocol": u"TCP", u"port": 10000},
                {u"name": u"storage", u"protocol": u"TCP", u"port": 10001},
            ],
        }
    )



def new_service(namespace, model):
    return _customer_grid_service(model).transform(
        [u"metadata", u"namespace"], namespace,
    )


def autopad_b32decode(s):
    padding = 8 - (len(s) % 8)
    s += u"=" * padding
    v = b32decode(s.upper())
    return v
