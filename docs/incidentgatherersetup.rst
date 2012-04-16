============================
What's an Incident Gatherer?
============================


 The incident gatherer is a process that runs on
 "monitoring.leastauthority.com". It connects to a number of tahoe-lafs nodes
 and, when they have an incident to report, it receives the contents of the
 incident from them and writes it into a file in the local filesystem.

 Potentially other gatherers will run there in the future. These gatherers
 have their own user account: "``gatherer``".

 An incident gatherer that monitors tahoe nodes need not be colocated with a
 ``tahoe`` repository, or ``tahoe`` process.  ``foolscap`` and ``twistd`` (upon which
 ``foolscap`` depends) are sufficient to run an incident gatherer. The LAE
 monitoring server does not have a copy of ``tahoe``.


HOWTO Set One Up:
=================

  (1) Get access to a "monitoring" persistently connected server, with sufficient memory to store incidents.
  (2) Set up a user "``gatherer``"
  (3) Make sure ``python-foolscap`` is installed
  (4) ``cd /home/gatherer && flogtool create-incident-gatherer incident``
  (5) ``cd /home/gatherer/incident && twistd -y gatherer.tac``
  (6) Obtain <FURL> from: "``/home/gatherer/log_gatherer.furl``"
  (7) On all monitored tahoe nodes set "``tahoe.cfg``" to contain: "``log_gatherer.furl = <FURL>``"


Config Tahoe Nodes to be Gathered:
==================================

  To set storageservers to be gathered by an incident gatherer (step 7 above), write a
  script (the original suffered an early demise) in the ``leastauthority.com`` directory.

  This script should:

   (1) obtain pub IPs of the hosting machines, from ``customerinfo.csv``
   (2) be able to ssh into the storageserver hosting machines, using ``EC2adminkeys2.pem``
   (3) use the ``setremoteconfigoption`` function of the ``lae_automation.server`` module.
   (4) the configuration to set is in the ``node`` section of ``tahoe.cfg`` it is "``log_gatherer.furl``".
   (5) a helper function in ``lae_automation/server.py`` named ``setremoteconfigoption`` is designed to set arbitrary (section, option, value) tuples in .ini style config files and is quite handy for this task!
   (6) the above function will restart the host, if you don't use it, you'll have to restart the server some other way!
