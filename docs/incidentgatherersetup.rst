==========================
Setup of Incident Gatherer
==========================

 The incident gatherer (and potentially other gatherers in the future) lives
 on monitoring.   The gatherers have their own user "``gatherer``".

 The file "``misc/incident-gatherer/support_classifiers.py``" appears to have
 been renamed "``misc/incident-gatherer/classify_tahoe.py``".

 It's worth noting that an incident gatherer monitoring tahoe nodes need not
 be colocated with a tahoe repo.  foolscap and twistd (upon which foolscap
 depends) are sufficient to run an incident gatherer.  Perhaps in the
 possibly ``classify_tahoe.py`` should be moved into the foolscap package?
 The correct disposition is unclear to me.

Names?
------
 I am confused by docs/logging.html, it states:::
  There are two kinds of gatherers: "log gatherer" and "stats gatherer".
 but then the next section is titled "``Incident Gatherer``".
 Incident gatherers are a type of log gatherer.

Which invocation?
-----------------
 flogtool create-incident-gatherer WORKDIR && cd WORKDIR && twistd -y gatherer.tac
 vs.
 flogtool create-incident-gatherer WORKDIR && cd WORKDIR && tahoe start
 Maybe it would be better to always use the former?

 The doc must tell the user to restart their storageserver!

HOWTO Setup An Incident Gatherer:
---------------------------------

  (0) Get access to a monitoring persistently connected etc. "monitoring" server.
  (1) Set up a user "gatherer"
  (2) Make sure python-foolscap is installed
  (3) ``cd /home/gatherer && flogtool create-incident-gatherer incident``
  (4) ``cd /home/gatherer/incident && twistd -y gatherer.tac``
  (5) Obtain <FURL> from: "``/home/gatherer/log_gatherer.furl``"
  (6) On all monitored tahoe nodes set "``tahoe.cfg``" to contain: "``log_gatherer.furl = <FURL>``"

Is the tahoe.cfg supposed to say incident_gatherer.furl = ??  No. The answer
is that log_gatherer.furl is the correct field to set for the incident
gatherer's furl.


Automated Setup:
================

  To set a Pub IP address indexed list of storageservers to be gathered by an
  incident gatherer, use the gatherincidentconfigs.py script in the
  leastauthority.com directory.  This script uses the setremoteconfigoption
  function of the lae_automation.server module.  It expects to be able to ssh
  using a key located in a file offered as its first argument.  It expects to
  be given a csv file containins pub IPs as the last element of each line as
  its second argument.
