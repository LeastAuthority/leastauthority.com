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


Testing That The Incident Gatherer is Working:
==============================================

To test that the incident gatherer that is registered with particular
tahoe-node is working follow this recipe:

 (1) On ``monitoring`` run: ``cd /home/gatherer/incident && watch 'ls -lart incidents classified'``
 (2) On ``monitoring`` run: ``cd /home/gatherer/incident && tail -f twistd.log``
 (3) On the Host EC2 run: ``cd /home/customer/storageserver && foolscap tail private/logport.furl``
 (4) On the Host EC2 change ``web.port =`` to ``web.port = 3456``
 (5) On the Host EC2: ``cd /home/customer && ./restart.sh``
 (6) On the Host EC2: ``curl --data flop localhost:3456/report_incident``.


========================
What's a Stats Gatherer?
========================

The stats gatherer (in contrast to the incident gatherer) *is* a tahoe node.

HOWTO SetUp A Stats Gatherer:
=============================

 (1) ssh to "monitoring"
 (2) install darcs
 (3) as user "gatherer": ``cd /home/gatherer/ && darcs get --lazy https://tahoe-lafs.org/source/tahoe-lafs/ticket999-S3-backend LAFS_source``
 (4) ``cd /home/gatherer/LAFS_source/``
 (5) ``python ./setup.py build``
 (6) ``cd /home/gatherer && mkdir stats && cd /home/gatherer/stats``
 (7) ``/home/gatherer/LAFS_source/bin/tahoe create-stats-gatherer --node-directory=/home/gatherer/stats``
 (8) ``/home/gatherer/LAFS_source/bin/tahoe start /home/gatherer/stats``
 (9) ``cat stats_gatherer.furl`` and place the value into the appropriate field in the website configuration json file.

.. [1] For NAT traversal we use ``monitoring.leastauthority.com`` in place of an IP address.
.. [2] Ours is: ``CENSORED!!``
