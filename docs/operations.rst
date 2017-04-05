Operations
==========

Front Matter
~~~~~~~~~~~~

Scope
-----

This document outlines the operating procedure for S4, LeastAuthority's AWS-hosted Tahoe-LAFS service.

Audience
--------

This document is intended for individuals to interact with the service's deployment.
The primary interaction expected is to deploy new versions of the service software.

Caveats
-------

The implementation of the service leverages Kubernetes heavily.
Some familiarity with Kubernetes will be helpful in understanding the documentation.
An attempt has been made to explain many relevant Kubernetes concepts here.
However, for a more complete understanding, see the official Kubernetes documentation.

Overview
~~~~~~~~

The service operates as a web server which accepts end-user requests for content and registration.
Web requests for registration generate a foolscap request to a flapp server responsible for that process.
The flapp server launches a "full signup" script.
The signup script submits subscription details to the *subscription manager* which maintains a subscription database.
Separately, a *subscription converger* polls the *subscription manager*.
It provisions resources (such as Tahoe-LAFS nodes) for new subscriptions it finds there and cleans up resources for no-longer-active subscriptions.
Each subscription has a large portion of dedicated resources:
the Kubernetes cluster (and its compute, memory, and network components) is a shared resource but little else is.

Top-Down Components
~~~~~~~~~~~~~~~~~~~

Looking at the service from the top down, the components involved in operating it are as follows.

Web Server
----------

Twisted Web runs in a container serving up static content residing in that same container.
It serves website content specifically related to the signup process
(the rest of the leastauthority.com website is served elsewhere).
The container is run by Docker on a Kubernetes worker node on an EC2 instance.
The server uses filesystem storage to persist access logs and to read state to find the Flapp server.
The server also manages the Stripe interaction for billing new signups.
It is part of the s4-infrastructure pod.
The server is implemented in ``lae_site``.

Flapp Server
------------

A Foolscap application server runs in a container handling signup requests from the web server.
The container is run by Docker on a Kubernetes worker node on an EC2 instance.
The server uses filesystem storage to persist logs and other signup details.
It is part of the s4-infrastructure pod.
The application logic for the signup process is in ``full_signup_docker.sh``.

Grid Router
-----------

All client access to Tahoe-LAFS resources are mediated by this Foolscap proxy.
Kubernetes is configured to route all S4 Tahoe-LAFS connections to this proxy.
This proxy maintains an awareness of which Tahoe-LAFS introducers and storage servers are running and to whom they belong.
It determines which Tahoe-LAFS node any particular connection is attempting to reach and then proxies it to the appropriate container.
It is part of the s4-infrastructure pod.
The grid router is implemented in ``grid_router``.

Subscription Manager
--------------------

The subscription manager maintains a database of S4 subscriptions.
It presents an HTTP-based API for creating, retrieving, and de-activating them.
It is part of the s4-infrastructure pod.
The subscription manager is implemented in ``lae_automation/subscription_manager.py``.

Subscription Converger
----------------------

The subscription converger updates Kubernetes configuration to reflect active subscriptions.
For example, when a new subscription is created:, the subscription converger:

  * finds it in the subscription manager database
  * notices that resources to service that subscription do not exist
  * creates Kubernetes ConfigMap and Deployment to provide Tahoe-LAFS nodes for the subscription
  * creates AWS Route53 record for the domain name in the subscription's furl
  * etc

It also performs the reverse process when it finds resources for which there is no active subscription.
At this time, the subscription converger will *not* delete the S3 bucket storing Tahoe-LAFS shares
(as a precaution against accidentally destroying important customer data).
It is part of the s4-infrastructure pod.
The subscription converger is implemented in ``lae_automation/subscription_converger.py``.

Tahoe-LAFS Introducer
---------------------

A Tahoe-LAFS introducer runs in a per-customer container.
The container is managed by a per-customer Kubernetes Deployment that is created by the *subscription converger*.
This container mediates access to the customer's storage service.
The container contains vanilla Tahoe-LAFS.
The container is defined by ``docker/Dockerfile.tahoe-introducer``.

Tahoe-LAFS Storage Service
--------------------------

A Tahoe-LAFS storage service runs in another per-customer container.
The container is managed by a per-customer Kubernetes Deployment that is created by the signup process.
This container mediates access to the customer's S3 bucket.

Bottom-Up Components
~~~~~~~~~~~~~~~~~~~~

Looking at the service from the bottom up, the components involved in operating it are as folllows.

AWS EC2
-------

The service runs on several EC2 instances.
The EC2 instances used to run infrastructure are disposable.
So long as enough of them are running to handle the service load, the service should operate properly.
The EC2 instances run within a LeastAuthority-owned AWS account.

Persistent state related to the operation of the service
(such as customer subscriptions, server logs, monitoring data - **not** customer-owned, Tahoe-LAFS-managed data)
is stored on EBS instances attached to the EC2 instances.
The EBS instances belong to a LeastAuthority-owned AWS account.

Customer requests are serviced by containers which share the EC2 instances.
Each customer has their own containers.
Customer data is stored on S3, not instance storage of EBS.
Per-customer Kubernetes Deployments will automatically recreate destroyed customer containers.

AWS EC2 is not a very tightly coupled component of the system.
The intent is that Kubernetes is the platform.
The hope is that if Kubernetes were deployed on a different cloud provider, everything would continue to work.
This is probably not the case but perhaps the necessary porting would be minimal.

AWS S3
------

Tahoe-LAFS services are configured with the S3 storage backend and therefore use S3 buckets.
All customer-owned data resides in these S3 buckets.
The S3 buckets belong to a LeastAuthority-owned AWS account.

AWS S3 is a fairly tightly coupled component of the system.
The Tahoe-LAFS S3 storage backend is leveraged extensively.
Customer data is stored in S3 buckets.
A transition to another storage system requires both code changes and data migration.

Docker Registry
---------------

The service launches Docker containers using images hosted on a local Docker registry.

It is possible for the images to be signed and the signatures to be checked before the containers are started.
This is not currently implemented.

It is possible for images to be stored on an encrypted EBS volume.
This is not currently implemented.

Kubernetes
----------

Some errors are expected as Kubernetes resources are created.
Ordering of creation of different components is not enforced.
The cluster should converge on a working state as dependencies get created.

kops
====

The Kubernetes cluster is created and managed using kops_:
"Production Grade K8s Installation, Upgrades, and Management."


Consult the kops documentation to learn more about its operation.

The infrastructure services are defined by the yaml files referenced in the above transcript.
To ensure repeatability, the services should always be deployed from these version-controlled artifacts.
The Kubernetes dashboard provides features for directly editing the configuration of the services.
This can be useful for experimentation.
However, after the experiment concludes, modified resources should always be deleted and re-created from the version controlled artifact.
This will ensure repeatability of the changes.
See `Update Version of leastauthority.com`_ for an example of a properly version-controlled deployment change.

Stripe
------

Stripe is responsible for billing active users.
Stripe's JavaScript client is used to send billing details directly to Stripe.
The only automated Stripe interaction is signup.
Cancellation and other interactions are currently manual processes.

Ops Tasks
~~~~~~~~~

Deploy Kubernetes Cluster
-------------------------

Infrequently, it will be necessary to create a Kubernetes cluster.
Ideally, this should happen once and never again.
However, it may be useful to create throw-away Kubernetes clusters for certain testing.
For example, testing an upgrade of a Kubernetes component or testing a new kind of ops task.
For the near future, a single production Kubernetes cluster should be able to handle our tasks.
This includes deploying staging versions of the leastauthority.com website.

The tools required to deploy a Kubernetes cluster are::

  * `kops`_
  * `kubectl`_
  * `awscli`_
  * `jq`_ (unless you're content parsing JSON with your eyeballs)

The first step is to set up the S3 bucket where ``kops`` will store Kubernetes cluster configuration.
Multiple clusters can share a single bucket so if one already exists, it can be re-used.
If one does not exist, make it (configure awscli[#awsconf]_ first, if necessary)::

  aws s3 mb s3://some-bucket-maybe-shared

Either way, set the bucket name in the environment so ``kops`` can find it::

  export KOPS_STATE_STORE=s3://some-bucket-maybe-shared

Then create the configuration, stored in that bucket, for a brand new cluster::

  kops create cluster \
      --zones us-east-1c \
      --name useast1.staging.leastauthority.com \
      --cloud aws \
      --dns-zone staging.leastauthority.com \
      --ssh-public-key key.pub

Pick whatever AWS zones and cluster name you like.
Stick to AWS because LeastAuthority.com has various AWS dependencies.
The ``--dns-zone`` should be a zone managed by `AWS Route53`_.
The SSH public key (RSA only due to AWS restrictions) will be installed on the AWS instances that host the Kubernetes cluster.
The security of this key is critical to the security of the cluster.
Use a good key and keep it safe.


The next step is to customize the cluster configuration.
For example, customize the size of the master or the number of workers (set ``${EDITOR}``!)::

  kops edit ig \
      --name useast1.staging.leastauthority.com master-us-east-1c
  kops edit ig \
      --name useast1.staging.leastauthority.com nodes

Once satisfied with the configuration, deploy it::

  kops update cluster useast1.staging.leastauthority.com --yes

The command completes before the cluster has settled.
Monitor the status with ``kubectl``::

  kubectl get nodes

``kubectl`` commands will fail, usually with some kind of "connection refused" error, during the early stage of cluster startup.
After the expected number of nodes and masters have appeared, install the Kubernetes dashboard::

  kubectl create -f https://rawgit.com/kubernetes/dashboard/master/src/deploy/kubernetes-dashboard.yaml

Then monitor the status of it::

  kubectl --namespace kube-system get pods --selector app=kubernetes-dashboard

At this point, you have a basically usable Kubernetes cluster.
You can interact with the dashboard's web interface at ``https://api.<cluster name>/ui``.
Credentials for the dashboard can be found (and modified) in the "kubecfg"::

  kubectl config view

Upgrade Kubernetes
~~~~~~~~~~~~~~~~~~

See https://github.com/kubernetes/kops/blob/master/docs/upgrade.md

Deploy LeastAuthority.com (From Scratch)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LeastAuthority.com comprises two Kubernetes services and three persistent volumes.
The services::

  #. A private Docker registry which hosts the secrets-containing infrastructure images.
  #. The web and flapp servers which serves the website to users and implements signup.

The volumes must be provisioned manually.
Make sure they are in the same availability zone as the cluster
(as defined in the ``kops create cluster`` command when it was created).
Create a volume for the Docker registry.
Make it around ten times the size of the site's infrastructure image [#volumesize]::

  aws ec2 create-volume --availability-zone us-east-1c --size 8

Take note of the ``VolumeId`` of the created volume.
Edit ``k8s/registry.yaml``.
Put the ``VolumeId`` into the ``volumeID`` field of the ``leastauthority-tweaks-kube-registry-pv`` resource.
If you pick a different volume size, also update the ``storage`` field to match.

Repeat this procedure to create volumes for the web and flapp servers.
These servers write human-readable logs and signup information to the volumes.
One GiB each is most likely sufficient.
Put the ``VolumeId`` values for these volumes into ``infrastructure.yaml``,
in the ``infrastructure-web-pv`` and ``infrastructure-flapp-pv`` resources.

You can now deploy the registry and web and flapp servers::

  kubectl create -f "k8s/"

.. note::
   This also creates an "image building" deployment which is not necessary for production operation but is helpful later in the setup process.
   You can skip creating this by identifying the registry and infrastructure configurations specifically, rather than the entire ``k8s`` directory.

After the infrastructure resources are configured on the cluster, it's time to configure their credentials.
The infrastructure resources need things like a private key and x509 certificate, an AWS API keys, etc.
These are exposed via Kubernetes "secrets" resources.
To create these resources, get a checkout of the ``secret_config`` repository.
Then, run the ``k8s/create-secrets`` script it contains.
By default, this creates resources in a "staging" namespace.
To deploy production secrets, override this setting with ``--namespace default``.

.. _build-docker-images:

The next step is to build the Docker images for the site.
You can do this locally or you can use the k8s cluster to do it.

Locally
-------

Use the build script in your ``leastauthority.com`` checkout to build the Docker images using your local Docker daemon::

  docker/build.sh

Then set up a proxy to the private Docker registry.
First, find the name of the registry's pod::

  x=($(kubectl --namespace leastauthority-tweaks -o json get pods | jq -r .items[0].metadata.name))
  name=${x[0]}
  port=${x[1]}

Then forward the port (stays in the foreground)::

  kubectl port-forward $name $port

Then tag the images so they can be pushed to the registry::

  docker tag leastauthority.com/web 127.0.0.1:$port/leastauthority/web
  docker tag leastauthority.com/flapp 127.0.0.1:$port/leastauthority/flapp

You can also put a tag in these tags (no joke).
Read more about image tags in the official Docker documentation.

*Then* push the images::

  docker push 127.0.0.1:$port/leastauthority/web
  docker push 127.0.0.1:$port/leastauthority/flapp

If you gave them a special tag, be sure to push that tag.
You are now done with the forwarded port and can kill ``kubectl``.

Using K8S
---------

Use the build script in your ``leastauthority.com`` checkout to build the Docker images in a pod running on a Kubernetes cluster::

  export K8S_POD=$(kubectl -o json get pods -l run=image-building | jq -r '.items[0].metadata.name')
  export GIT_BRANCH=master
  k8s/build.sh

This will run steps similar to those for the local build described above but it will run them in a container on the k8s cluster.
You can build images for a different git branch by changing the value of ``GIT_BRANCH``.
The current ``HEAD`` revision of the branch as found on GitHub is what is built into the image.

Check for Success
-----------------

After building the images locally or on k8s, you're almost done.
Most likely, Kubernetes tried to start things up already and failed because the images were missing.
This is okay.
It will keep trying until things succeed (now that you've pushed the images).
You can monitor the progress via the dashboard web interface or::

  kubectl get pods --selector provider=LeastAuthority

After the pods are running, you can use the AWS-provided LoadBalancer endpoint to access the service.
Look it up using::

  kubectl describe services --selector provider=LeastAuthority

The ``LoadBalancer Ingress`` line should look like a domain name.
Hit it via HTTPS.

If you want, you can now create a CNAME for that domain to make the site accessible more easily.

You're done.

Update Version of leastauthority.com
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. _update-lae:

`Build new Docker images for the new version of leastauthority.com<build-docker-images>`_.
Edit the infrastructure deployment file, ``k8s/infrastructure.yaml``.
Change the Docker image used by the containers to reference the tag of the newly built images
(commit, PR, merge, etc).
Use ``kubectl`` to apply the changes and begin a rolling update to deploy them::

  kubectl apply -f k8s/infrastructure.yaml

React to Increased Load
~~~~~~~~~~~~~~~~~~~~~~~

If the Kubernetes cluster is suffering due to high load,
it may be possible to improve the matter by adding additional Kubernetes worker nodes.

.. attention::
   The web server and signup processes do not support any kind of horizontal scale-out at this time.
   Their performance will not be improved by adding additional worker nodes.
   Instead, consider moving them to a larger EC2 instance size.

The cluster's ``nodes`` configuration defines a minimum and maximum number of workers.
You can change these values::

  kops edit ig \
      --name useast1.staging.leastauthority.com nodes

And then deploy the change::

  kops update cluster useast1.staging.leastauthority.com --yes

It is up to the Kubernetes scheduler to re-balance load on the cluster.

.. note::
   I don't actually know how well it does that, particularly for existing loads (ie, containers).
   -jean-paul

.. warning::
   Reducing the number of worker nodes may temporarily disrupt service as pods running on those nodes die and are then rescheduled elsewhere.

Customer Subscription Cancellation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This process `should be automated https://github.com/LeastAuthority/leastauthority.com/issues/361`_.
Until it is, the Stripe subscription and AWS resources must be adjusted directly.

#. Cancel the Stripe subscription in the Stripe admin web interface.
#. Get a shell in the flapp container of the s4-infrastructure pod.

   #. ``kubectl get -o json pods -l provider=LeastAuthority,app=s4,component=Infrastructure | jq '.items[].metadata.name'``
   #. ``kubectl exec -it <pod name> -c flapp -- /bin/bash``

#. Find the customer's directory

   #. ``cd /app/data/secrets``
   #. ``grep -r <email address> ./``

#. Find the EC2 ``instance_id`` associated with the customer

   #. ``grep instance <customer directory>/signup_logs``

#. Terminate that EC2 instance (AWS S4 customer account) using the AWS Web Console, CLI tools, or other means.
   e.g.:

   #. ``aws ec2 terminate-instances --instance-ids i-...``

#. Move the customer's directory to the ``cancelled`` directory

   #. ``mv -iv ... /app/data/secrets/cancelled``

.. _kops: https://github.com/kubernetes/kops
.. _kubectl: http://kubernetes.io/docs/user-guide/kubectl-overview/
.. _awscli: https://aws.amazon.com/cli/
.. _AWS Route53: https://aws.amazon.com/route53/

.. [#awsconf] The ``AWS_PROFILE`` environment variable may be helpful in this.
.. [#volumesize]
   The images are around 500MiB.
   There are two of them but they share most of their storage.
   For upgrades, it will be beneficial to have two different versions of the images in the registry at the same time.
   Different versions of the images will probably also share most of their storage.
   Ten times the image seems like it should be plenty without being wasteful.
   It can always be made larger later.
