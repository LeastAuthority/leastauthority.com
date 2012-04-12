============================
What's an Incident Gatherer?
============================

 The incident gatherer is a process that runs on
 "monitoring.leastauthority.com". Potentially other gatherers will run there
 in the future. These gatherers have their own user account: "``gatherer``".

 An incident gatherer that monitors tahoe nodes need not be colocated with a
 ``tahoe`` repository, or ``tahoe`` process.  ``foolscap`` and ``twistd`` (upon which
 ``foolscap`` depends) are sufficient to run an incident gatherer. The LAE
 monitoring server does not have a copy of ``tahoe``.


HOWTO Set One Up:
=================

  (0) Get access to a "monitoring" (persistently connected etc.) server.
  (1) Set up a user "``gatherer``"
  (2) Make sure ``python-foolscap`` is installed
  (3) ``cd /home/gatherer && flogtool create-incident-gatherer incident``
  (4) ``cd /home/gatherer/incident && twistd -y gatherer.tac``
  (5) Obtain <FURL> from: "``/home/gatherer/log_gatherer.furl``"
  (6) On all monitored tahoe nodes set "``tahoe.cfg``" to contain: "``log_gatherer.furl = <FURL>``"


Config Tahoe Nodes to be Gathered:
==================================

  To set storageservers to be gathered by an incident gatherer, write a
  script in the ``leastauthority.com`` directory.

  This script should:

   (1) obtain pub IPs of the hosting machines, from ``customerinfo.csv``
   (2) be able to ssh into the storageserver hosting machines, using ``EC2adminkeys2.pem``
   (3) use the ``setremoteconfigoption`` function of the ``lae_automation.server`` module.


Miscellany:
===========

 The file "``misc/incident-gatherer/support_classifiers.py``" appears to have
 been renamed "``misc/incident-gatherer/classify_tahoe.py``".

Names?
------
 I am confused by docs/logging.html, it states:

  ``There are two kinds of gatherers: "log gatherer" and "stats gatherer".``

 but then the next section is titled "``Incident Gatherer``".
 Incident gatherers are a type of log gatherer.

Which invocation?
-----------------
 flogtool create-incident-gatherer WORKDIR && cd WORKDIR && twistd -y gatherer.tac
 vs.
 flogtool create-incident-gatherer WORKDIR && cd WORKDIR && tahoe start
 Maybe it would be better to always use the former?

 The doc must tell the user to restart their storageserver!


 Is the tahoe.cfg supposed to say incident_gatherer.furl = ??  No. The answer
 is that log_gatherer.furl is the correct field to set for the incident
 gatherer's furl.
