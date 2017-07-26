Development
===========

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
