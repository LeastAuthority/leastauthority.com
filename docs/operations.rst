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

AWS
---

The service runs on several EC2 instances.
The EC2 instances are disposable.
So long as enough of them are running to handle the service load, the service should operate properly.
The EC2 instances run within a LeastAuthority-owned AWS account.

Persistent state related to the operation of the service
(such as customer subscriptions, server logs, monitoring data - **not** customer-owned, Tahoe-LAFS-managed data)
is stored on EBS instances attached to the EC2 instances.
The EBS instances belong to a LeastAuthority-owned AWS account.

Tahoe-LAFS services are configured with the S3 storage backend and therefore use S3 buckets.
All customer-owned data resides in these S3 buckets.
The S3 buckets belong to a LeastAuthority-owned AWS account.

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


.. _CoreOS (stable) cluster: https://coreos.com/os/docs/latest/booting-on-ec2.html
