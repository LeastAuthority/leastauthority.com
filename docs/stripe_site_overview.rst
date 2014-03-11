lae_site/main.py is called by leastauthority.com/runsite.sh

lae_site/main.py imports function start from handlers/subscription_complete.py,
and calls it (which returns a deferred).

function start assigns variables:  "all_subscribed", and "subscribed_confirmed",
and "flappcommand" and makes them "globals"

  "all_subscribed" is the set of subscription-unique_ids that have been
  returned from successful subscriptions

  "subscribed_confirmed" is the set of subscription-unique_ids associated
  with email addresses that have had confirmation emails sent to them

  "flappcommand" is the service that invokes full_signup.py (see below)

after lae_site/handlers/subscription_complete.py --> start returns a deferred
lae_site/main.py --> main is invoked as the next callback on that deferreds
chain

lae_site/main.py --> main first processes command line args which configure
ports

  Then it sets up logging. (See logging.basicConfig and website/site.out.)

  It then uses handlers/__init__.py --> make_site to create a
  twisted.web-served Jinja2-template-rendered site.

  It then log which port the site is serving.

  If SSL/TLS then it logs the fact, ensures that cert(s) and server key are
  available and sets up a twisted.internet.ssl factory to handle SSL/TLS.

  If the site is so configured it listens on port 80 and redirects to 443 by
  default or from and to specified non-default ports such that port 80
  requests are redirected to 443 and hence through TLS.

When HTTP requests are submitted to the site, handlers specified by make_site
service those requests whose path elements they match:

e.g. https://leastauthority.com/collect-email is handled by:

handlers/devpay_complete.py --> CollectEmailHandler (a twisted.internet.web
"Resource" object)

and

https://leastauthority.com/subscribing is handled by:

handlers/subscribing.py --> SubscriptionSubmitHandler

SubscriptionSubmitHandler handles GETs by rendering:

lae_site/templates/subscription_signup.html

The <form id="payment-form" element in subscription_signup.html has an action
attribute with value "/subscription_complete", therefore when the form
completes the actions specified in its body (which includes verifying a cc
with Stripe), an HTTP request is submitted to the url:

https://leastauthority.com/subscription_complete

The request is a POST method and the body of the request includes the data
returned by Stripe, i.e. a stripe.Customer
(https://stripe.com/docs/api#customers).

lae_site/handlers/subscription_complete.py -->
SubscriptionReportHandler.render_POST does the following:

   logs secret data in a website/secrets/TIMESTAMP_CUSTOMERID file
   logs nonsecret data in website/signup_logs/TIMESTAMP_CUSTOMERID
   logs the:

   timestamp, subcription_id, Plan, name, email address, PGP pub key

   to the: "subscriptions.csv" file

   logs the:

   timestamp, activation_state, subscription_id, Plan, name, email address

   to the: "service_confirmed.csv" file

   invokes the signup service that was registered by: "start" from handlers/subscription_complete.py
   by running flappcommand.run(relevant arguments) NOTE: recall flappcommand
   is global.

The flappcommand itself was created when the server was initialized (see
lae_automation/server.py) it runs leastauthority.com/full_signup.py.

leastauthority.com/full_signup.py is invoked by the flappserver with the
argument passed to flappcommand.run as if the parameters of flappcommand.run
were command line parameters to full_signup.py.  <-- The easier to debug you
with, my sweet.
