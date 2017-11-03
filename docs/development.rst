Local Deployments
=================

Front Matter
~~~~~~~~~~~~

Scope
-----

This document outlines the procedure for running the S4 backend service on a local host for testing and development purposes.

Audience
--------

This document is intended for individuals conducting testing and development on the S4 backend itself.
The primary interaction expected is to deploy in-development versions of the service software.
Manual testing of scenarios that are difficult to properly automatically test can then be conducted.

Overview
~~~~~~~~

This document outlines two different ways to run the various pieces of S4 backend service software.
The first is by running the processes natively ("native").
The second is by running a local Kubernetes cluster ("kubernetes").

Native Deployment
~~~~~~~~~~~~~~~~~

Local Development Environment
-----------------------------

Install any "native" dependencies that the Python packaging/installation toolchain cannot deal with::

  sudo apt install libffi-dev libssl-dev

Use ``virtualenv`` to get an isolated Python environment to run S4 components::

  virtualenv --python=python2 ~/Environments/leastauthority.com
  . ~/Environments/leastauthority.com/bin/activate

Check out the source from GitHub and install it in the *virtualenv*::

  git clone git@github.com:LeastAuthority/leastauthority.com
  cd leastauthority.com
  ./test-tools/install-testing

Run the test suite to verify the environment is in good condition::

  ./test-tools/run-testing


S4 Subscription Manager
~~~~~~~~~~~~~~~~~~~~~~~

The *subscription-manager* container of the ``subscription-manager`` *Deployment* in ``k8s/infrastructure.yaml`` provides the canonical definition of the **S4 Subscription Manager** is run.
However, here is a slightly simplified and reformatted version that probably works.

::

   twist \
     --log-format=text \
     s4-subscription-manager \
     --domain ${DOMAIN} \
     --state-path /tmp/subscriptions \
     --listen-address tcp:8001

For provisioning of subscriptions contained by this manager to succeed
(such that the Tahoe-LAFS for the subscription is actually usable)
``DOMAIN`` must be set to a Route53-managed domain which other components of the system are also using.
For any testing short of that, any value for ``DOMAIN`` should be fine.


S4 Signup Server
~~~~~~~~~~~~~~~~

To run the S4 signup server at all requires a number of parameters,
some of which correspond to API keys or other credentials.
For local development purposes, usable values for these parameters can be extracted from ``k8s/secrets.staging.enc.yaml`` using ``sops``.

::

   sops -d k8s/secrets.staging.enc.yaml

The decrypted values must also be base64 decoded before they are usable.
Set some of these values as environment variables for use in later commands::

  STRIPE_PRIVATE_KEY=... stripe-private.key ...
  STRIPE_PUBLISHABLE_KEY=... stripe-publishable.key ...


The ``web`` *container* of the ``s4-signup`` *Deployment* in ``k8s/infrastructure.yaml`` provides the canonical definition of how the **S4 Signup Server** is run.
However, here is a slightly simplified and reformatted version that probably works.

::

   python -m lae_site.main \
     --wormhole-result-path=/tmp/wormhole-claims.jsons \
     --secure-port tcp:8000 \
     --stripe-secret-api-key-path <(echo ${STRIPE_PRIVATE_KEY}) \
     --stripe-publishable-api-key-path <(echo ${STRIPE_PUBLISHABLE_KEY}) \
     --site-logs-path /tmp/lae_site.json \
     --subscription-manager ${SUBSCRIPTION_MANAGER_URL} \
     --metrics-port tcp:9000

To be able to successfully submit the signup form,
``SUBSCRIPTION_MANAGER_URL`` will need to be set to the address of a usable subscription manager.
For example,
if the subscription manager is running on localhost and was given a ``--listen-address`` of ``tcp:8001``
then the value for ``SUBSCRIPTION_MANAGER_URL`` would be ``http://localhost:8001/``.

Kubernetes Deployment
~~~~~~~~~~~~~~~~~~~~~

A local deployment is made possible through the use of minikube.
Minikube is a tool for running Kubernetes on a single host through the use of virtual machines.
These instructions have been tested with minikube v0.23.

The Deployment
~~~~~~~~~~~~~~

Download and install minikube as appropriate for your platform.
Then use the following script as a guide for the subsequent steps necessary.

::

   $ minikube start
   ...
   Kubectl is now configured to use the cluster.
   $ IP=$(minikube ip)
   $ kubectl config get-contexts
   CURRENT   NAME         CLUSTER          AUTHINFO       NAMESPACE
   *         minikube     minikube         minikube
   ...
   $ export KUBECONFIG=~/.kube/staging.yaml:~/.kube/production.yaml:~/.kube/config
   $ ./ops/stage-current-HEAD minikube
   ...
   $ kubectl --context minikube get pod
   NAME                                      READY     STATUS    RESTARTS   AGE
   foolscap-log-gatherer-1614954323-96ngg    1/1       Running   0          3m
   grid-router-642627163-pj3fb               1/1       Running   0          3m
   grid-router-642627163-qp3xx               1/1       Running   0          3m
   s4-signup-292847253-1fwkv                 1/1       Running   0          3m
   subscription-converger-4212458664-4mnrx   1/1       Running   0          3m
   subscription-manager-137010057-6cxbz      1/1       Running   0          3m
   $ kubectl --context minikube get service s4
   NAME      CLUSTER-IP   EXTERNAL-IP   PORT(S)                      AGE
   s4        10.0.0.128   <pending>     443:**32096**/TCP,80:30324/TCP   3h
   $ curl --insecure https://${IP}:32096/configuration
   {"stripe-publishable-api-key": ...
   $

To enable web-based signup, you will also need to deploy a local instance of the website.
See the instructions in that repository for details.
You will also need to change the ``cross-domain`` secret in the minikube-based deployment::

  $ EDITOR=... kubectl --context minikube edit service s4

Change the value to the base64 encoding of ``http://localhost:8080``
(or whatever ``Origin`` header corresponds to the address you will run the website).
You can also use the base64 encoding of ``*`` to allow requests from any address.
