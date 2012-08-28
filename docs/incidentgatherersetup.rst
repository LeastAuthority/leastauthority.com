============================
What's an Incident Gatherer?
============================

 The incident gatherer is a process that runs on "monitoring.leastauthority.com".
 When 'registered' tahoe-lafs nodes have incidents to report they connect to
 it and send the contents of the incident report.  The gatherer writes
 the report into a file in the local filesystem.

 Potentially other gatherers will run on "``mu``" in the future. These gatherers
 have their own user account: "``gatherer``".

 An incident gatherer that monitors tahoe nodes need not be colocated with a
 ``tahoe`` repository, or ``tahoe`` process.  ``foolscap`` and ``twistd``
 (upon which ``foolscap`` depends) are sufficient to run an incident
 gatherer. The LAE monitoring server "``monitoring``" does not have a copy of
 ``tahoe``, for its incident gatherer.


HOWTO Set One Up:
=================

  (1) Get access to a "monitoring" persistently connected server, with sufficient memory to store incidents.
  (2) Make sure ``python-foolscap`` is installed
  (3) Set up a user "``gatherer``", and run subsequent steps as this user.
  (4) ``cd /home/gatherer && flogtool create-incident-gatherer incident`` [1]_
  (5) ``cd /home/gatherer/incident && twistd -y gatherer.tac``
  (6) Obtain <FURL> from: "``/home/gatherer/incident/log_gatherer.furl``" [2]_
  (7) On all monitored tahoe nodes set "``tahoe.cfg``" to contain: "``log_gatherer.furl = <FURL>``"


Config Tahoe Nodes to be Gathered:
==================================

.. _The source code for the above described script: ../registergatherer.py

To set storageservers to be gathered by an incident gatherer (step 7 above),
write a script (the original suffered an early demise) in the
``leastauthority.com`` directory.

This script should:

   (1) obtain pub IPs of the hosting machines, from ``customerinfo.csv``
   (2) be able to ssh into the storageserver hosting machines, using ``EC2adminkeys2.pem``
   (3) use the ``setremoteconfigoption`` function of the ``lae_automation.server`` module. (This function is idempotent.)
   (4) the configuration to set is in the ``node`` section of ``tahoe.cfg`` it is "``log_gatherer.furl``".
   (5) a helper function in ``lae_automation/server.py`` named ``setremoteconfigoption`` is designed to set arbitrary (section, option, value) tuples in .ini style config files and is quite handy for this task!
   (6) the above function will restart the host, if you don't use it, you'll have to restart the server some other way!

`The source code for the above described script`_.


Testing That The Incident Gatherer is Working:
==============================================

To test that the incident gatherer that is registered with particular
tahoe-node is working follow this recipe:

 (1) On ``monitoring`` run:
   ``cd /home/gatherer/incident && watch 'ls -lart incidents classified'``
 (2) On ``monitoring`` run:
   ``cd /home/gatherer/incident && tail -f twistd.log``
 (3) On the Host EC2 run:
   ``cd /home/customer/storageserver && foolscap tail private/logport.furl``
 (4) On the Host EC2 change ``web.port =`` to ``web.port = 3456``
 (5) On the Host EC2: ``cd /home/customer && ./restart.sh``
 (6) On the Host EC2: ``curl --data flop localhost:3456/report_incident``.




.. [1] For NAT traversal we use ``monitoring.leastauthority.com`` in place of an IP address.
.. [2] Ours is: ``CENSORED!!``
