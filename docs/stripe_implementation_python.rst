New Code
--------
 There are 10 modules which exist in the stripe branch, and have been changed
 relative to master.  They are:

 leastauthority.com/
  - full_signup.py
  - create_stripe_bucket.py
  - lae_automation/
     - initialize.py
     - server.py
     - signup.py
  - lae_site/
     - main.py
  - lae_site/handlers
     - subscribing.py
     - subscription_complete.py
  - lae_util/
     - flapp.py

Unittests
---------

 create_stripe_bucket.py
  doesn't need unittesting as it's not used by the automation in the server

 lae_site/handlers/subscription_complete.py
  needs the following tests:

   #. test that failure to return a token from stripe is properly handled
   #. test the stripe api key is not available in the repository
