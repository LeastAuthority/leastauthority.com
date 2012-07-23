How To Set Up Realtime Monitoring:
==================================

Each section gives a procedure for monitoring a separate component in the system.

This document assumes the monitoring agent has the appropriate permissions.

GateWay:
--------

(#) ``pushd ${GATEWAYDIR} && ${PATHTOBINTAHOE} restart ${GATEWAYDIR}``

(#) ``flogtool tail private/logport.furl``

(#) [Optional] In a second session: ``pushd ${GATEWAYDIR} && tail -f logs/twistd.log``

StorageServer:
--------------

(#) ``ssh -i ${SSHPRIVKEYFILE}.pem customer@${PUBIPOFTESTEC2}``

(#) ``pushd /home/customer/storageserver && flogtool tail private/logport.furl``

(#) [Optional] In a second session: ``pushd /home/customer/storageserver && tail -f logs/twistd.log``

Incident Gatherer:
------------------

(#) ``ssh gatherer@monitoring.leastauthority.com``

(#) ``pushd /home/gatherer/incident && tail -f twistd.log``

(#) [Optional] In a second session: ``watch 'ls -lart incidents/${TESTEC2TUBID}/ | tail -5'``
