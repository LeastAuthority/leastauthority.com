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

Flapp Server
------------

Tahoe-LAFS Introducer
---------------------

Tahoe-LAFS Storage Server
-------------------------

Bottom-Up Components
~~~~~~~~~~~~~~~~~~~~

Looking at the service from the bottom up, the components involved in operating it are as folllows.

AWS EC2
-------

The service runs on several EC2 instances.
The EC2 instances are disposable.
So long as enough of them are running to handle the service load, the service should operate properly.
The EC2 instances run within a LeastAuthority-owned AWS account.

Persistent state related to the operation of the service
(such as customer subscriptions, server logs, monitoring data - **not** customer-owned, Tahoe-LAFS-managed data)
is stored on EBS instances attached to the EC2 instances.
The EBS instances belong to a LeastAuthority-owned AWS account.

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

AWS ECR
-------

The service launches Docker containers using images hosted on ECR.
The images are encrypted in ECR storage using AWS KMS.
The images are encrypted in transit from ECR using HTTPS.

It is possible for the images to be signed and the signatures to be checked before the containers are started.
This is not currently implemented.

CoreOS
------

The EC2 instances run a `CoreOS (stable) cluster`_.
CoreOS receives and applies updates automatically.

Kubernetes
----------

https://coreos.com/kubernetes/docs/latest/kubernetes-on-aws.html
http://kubernetes.io/docs/user-guide/prereqs/

  aws kms create-key --description 'kube-aws assets'
  kube-aws init --cluster-name leastauthority-k8s-coreos-testing --external-dns-name $DOMAIN --region us-east-1 --availability-zone us-east-1a --key-name jeanpaul_meson_rsa --kms-key-arn $KMS_ARN
  kube-aws render
  kube-aws up

  kubectl create -f (all the yaml files!)

Stripe
------


.. _CoreOS (stable) cluster: https://coreos.com/os/docs/latest/booting-on-ec2.html
