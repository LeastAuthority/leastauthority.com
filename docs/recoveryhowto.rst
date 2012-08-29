
=======================
Disaster Recovery HOWTO
=======================

  The disaster recovery process consists of a series of function calls.
  These functions are used in the signup process.  For the recovery situation
  we are prepared for here the functions from "deploy_EC2_instance" to the
  end of the signup.py module must all be called for all destroyed
  server/accounts.

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

bounce_server:
~~~~~~~~~~~~~~

* *publichost*: AA

* *admin_privkey_path*: AA

* *privatehost*: in signup_logs, search for string "private_dns_name" this
   appears to be the empty string, at the moment (20120320).

* *useraccesskeyid*: in signup_logs, search for string "access_key_id"

* *usersecretkey*: in signup_logs, search for string "secret_key"

* *usertoken*: in signup_logs, search for string "UserToken"

* *producttoken*: This one's tricky we may have to correlate month of signup
   from the signup_logs-secrets file, and the access key id, and then look in
   lae_automation_config.json to get this.  Or:  Search for "Signing up
   customer for". **Or: ./activation_requests.csv**

* *bucketname*: AA

* *secretsfile*: In signup_logs, one per signup.  This is the name of the
   file inside signup_logs that contains the data we're using here.

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


code fragment:
~~~~~~~~~~~~~~

Code from signup.py::

        d2.addCallback(lambda ign: deploy_EC2_instance(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, amiimageid,
                                                       instancesize, bucketname, admin_keypair_name, instancename,
                                                       stdout, stderr))

        def _deployed(instance):
            d3 = task.deferLater(myclock, ADDRESS_DELAY_TIME, wait_for_EC2_addresses,
                                 ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, stdout, stderr,
                                 instance.instance_id)

            def _got_addresses(addresses):
                assert len(addresses) == 1, addresses
                (publichost, privatehost) = addresses[0]
                print >>stdout, "The server's public address is %r." % (publichost,)

                retries = 3
                while True:
                    try:
                        install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path, stdout, stderr)
                        break
                    except NotListeningError:
                        retries -= 1
                        if retries <= 0:
                            print >>stdout, "Timed out waiting for EC2 instance to listen for ssh connections."
                            raise TimeoutError()
                        print >>stdout, "Waiting another %d seconds..." % (LISTEN_POLL_TIME)
                        time.sleep(LISTEN_POLL_TIME)
                        continue

                furl = bounce_server(publichost, admin_privkey_path, privatehost, useraccesskeyid, usersecretkey, usertoken,
                                     producttoken, bucketname, stdout, stderr, secretsfile)

                append_record("serverinfo.csv", instance.launch_time, instance.instance_id, publichost)

                d4 = send_signup_confirmation(publichost, customer_name, customer_email, furl, customer_keyinfo, stdout, stderr)
                def _setup_monitoring(ign):
                    print >>stdout, "Setting up monitoring..."
                    notify_zenoss(publichost, zenoss_IP, zenoss_privkey_path)
                d4.addCallback(_setup_monitoring)
                return d4
            d3.addCallback(_got_addresses)
            return d3
        d2.addCallback(_deployed)
        return d2
    d.addCallback(_activated)
    d.addErrback(lambda f: send_notify_failure(f, customer_name, customer_email, logfilename, stdout, stderr))
    return d
