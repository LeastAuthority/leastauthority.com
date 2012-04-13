
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
