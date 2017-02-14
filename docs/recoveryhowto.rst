
=======================
Disaster Recovery HOWTO
=======================

  The disaster recovery process consists of a series of function calls.
  These functions are used in the signup process.  In the case of the
  recovery situation we are prepared for, the functions from
  "deploy_EC2_instance" to the end of the signup.py module must all be called
  for all destroyed server/accounts.

  This will need scripting if it's ever actually used, in which case this doc
  will serve as a useful reference.  I've listed the data necessary for these
  function calls and their location below.

  The last part of this document is just a verbatim snippet of the end of the
  signup.py module.  The recovery script will have a very similar
  appearance.

  - *"AA"*:  Where a data element is defined "As Above" this abbreviation is used.


necessary pieces of data for recovery, indexed by function:
-----------------------------------------------------------

deploy_EC2_instance:
~~~~~~~~~~~~~~~~~~~~

  This list comprises the arguments to deploy_EC2_instance, which will
  need to be invoked to construct a new EC2 for each customer during
  recovery.

* *ec2accesskeyid*: ../lae_automation_config.json

* *ec2secretkey*: ../ec2secret

* *EC2_ENDPOINT*: 'https://ec2.us-east-1.amazonaws.com/' or literal in signup.py

* *amiimageid*: ../lae_automation_config.json

* *instancesize*: ../lae_automation_config.json

* *bucketname*: starts with 'lae-' found only in signup_logs

* *admin_keypair_name*: ../lae_automation_config.json

* *instancename*: customer_email found in signup_logs.

install_server:
~~~~~~~~~~~~~~~

  This list is for the install server function.

* *publichost*: found only in signup_logs, among other places on lines with
   the pattern "public address"

* *admin_privkey_path*: ../lae_automation_config.json

* *monitor_pubkey*: ../lae_automation_config.json

* *monitor_privkey_path*: ../lae_automation_config.json

append_record:
~~~~~~~~~~~~~~

* *"serverinfo.csv"*: string literal

* *instance.launch_time*: in signup_logs, search for string "launch_time"

* *instance.instance_id*: in signup_logs, search for string "instance_id"

* *publichost*: AA

send_signup_confirmation:
~~~~~~~~~~~~~~~~~~~~~~~~~

* *publichost*: AA

* *customer_name*: Can/should we use the "instancename"?

* *customer_email*: same as "instancename"

* *furl*: in signup_logs, search for string "introducer.furl"

* *customer_keyinfo*: ./activation_requests.csv

notify_zenoss:
~~~~~~~~~~~~~~

* *publichost*: AA

* *zenoss_IP*: ../lae_automation_config.json

* *zenoss_privkey_path*: ../lae_automation_config.json

send_notify_failure:
~~~~~~~~~~~~~~~~~~~~

* *f*: The failure reason.  This will be generated during the recovery process.

* *customer_name*: AA

* *customer_email*: AA

* *logfilename*: In signup_logs, one per signup.  This is the name of the
   file inside signup_logs that contains the data we're using here.
