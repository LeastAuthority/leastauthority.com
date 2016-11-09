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

The implementation of the service is closely tied to the LeastAuthority infrastructure server.
This server runs the leastauthority.com website and has many functions unrelated to the S4 service.
Consequently, this document may be of interest to website maintainers as well.
However, the S4 service and the website should be split apart so this is no longer the case.

Overview
~~~~~~~~

The service operates as a web server which accepts end-user requests for content and registration.
Web requests for registration generate a foolscap request to a flapp server responsible for that process.
The flapp server launches a "full signup" script.
The signup script creates a new EC2 instance and deploys Tahoe-LAFS and other necessary software on it.
It then configures Tahoe-LAFS and notifies the user of the details of the newly provisioned service
(the introducer furl, for example).
The new EC2 instance continues to operate for as long as the user remains subscribed.
It provides service to that user alone.

Top-Down Components
~~~~~~~~~~~~~~~~~~~

Looking at the service from the top down, the components involved in operating it are as follows.

Web Server
----------

Twisted Web runs in a container serving up static content residing in that same container.
The container is run by Docker on a Kubernetes worker node on an EC2 instance.
The server uses filesystem storage to persist logs and other signup details.
It is part of the s4-infrastructure pod.
The server is implemented in lae_site.

Flapp Server
------------

A Foolscap application server runs in a container handling signup requests from the web server.
The container is run by Docker on a Kubernetes worker node on an EC2 instance.
The server uses filesystem storage to persist logs and other signup details.
It is part of the s4-infrastructure pod.
The application logic for the signup process is in full_signup_docker.sh.

Tahoe-LAFS Introducer
---------------------

A Tahoe-LAFS introducer runs on a per-customer EC2 instance that is created by the signup process.

Tahoe-LAFS Storage Service
--------------------------

A Tahoe-LAFS storage service runs alongside the introducer.
This mediates access to the customer's S3 bucket.

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

Each customer has their own EC2 instance.
Customer data is stored on S3, not instance storage of EBS.
Recreating a destroyed customer EC2 instance is possible, though it is a manual task.

AWS EC2 is not a very tightly coupled component of the system.
The signup process does integrate against the EC2 APIs directly.
A future direction is for customers to receive service from a container instead of an EC2 instance.
This will reduce coupling.

AWS S3
------

Tahoe-LAFS services are configured with the S3 storage backend and therefore use S3 buckets.
All customer-owned data resides in these S3 buckets.
The S3 buckets belong to a LeastAuthority-owned AWS account.

AWS S3 is a fairly tightly coupled component of the system.
The Tahoe-LAFS S3 storage backend us leveraged extensively.
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

Some errors are expected as kubernetes resources are created.
Ordering of creation of different components is not enforced.
The cluster should converge on a working state as dependencies get created.

kops
====

The Kubernetes cluster is created and managed using kops_:
"Production Grade K8s Installation, Upgrades, and Management."

A new Kubernetes cluster is deployed,
running the leastauthority.com infrastructure services,
using something like the following process::

  # Make the configuration for a new cluster
  export KOPS_STATE_STORE=s3://some-bucket-maybe-shared
  aws s3 mb $CONFIG_BUCKET
  kops create cluster \
      --zones us-east-1a \
      --name useast1.staging.leastauthority.com \
      --cloud aws \
      --dns-zone staging.leastauthority.com \
      --ssh-public-key key.pub

  # Customize it so it looks like we want
  kops edit ig \
      --name useast1.staging.leastauthority.com master-us-east-1a
  kops edit ig \
      --name useast1.staging.leastauthority.com nodes

  # Deploy the cluster
  kops update cluster useast1.staging.leastauthority.com --yes
  kubectl cluster-info
  kubectl create -f https://rawgit.com/kubernetes/dashboard/master/src/deploy/kubernetes-dashboard.yaml

  # Deploy leastauthority.com on it
  kubectl create -f registry.yaml
  kubectl create -f infrastructure.yaml

  # XXX Also push leastauthority docker images to cluster registry
  # XXX Get http basic auth admin password from `kubectl config view`.
  # XXX Probably change those credentials, too?

  # Find LoadBalancer Ingress (ELB endpoint) for the website
  kubectl describe services s4 | grep LoadBalancer
  # Create a CNAME from whatever.leastauthority.com to the LoadBalancer Ingress name

  # Change the number of worker nodes
  kops edit ig \
      --name useast1.staging.leastauthority.com nodes
  # Deploy the change
  kops update cluster useast1.staging.leastauthority.com --yes

  # Inspect the status of the infrastructure service
  kubectl describe services s4
  kubectl describe pods -l 'app=s4'
  ...

Consult the kops documentation to learn more about its operation.

The infrastructure services are defined by the yaml files referenced in the above transcript.
To ensure repeatability, the services should always be deployed from these version-controlled artifacts.
The Kubernetes dashboard provides features for directly editing the configuration of the services.
This can be useful for experimentation.
However, after the experiment concludes, modified resources should always be deleted and re-created from the version controlled artifact.
This will ensure repeatability of the changes.

Stripe
------

.. _kops: https://github.com/kubernetes/kops
