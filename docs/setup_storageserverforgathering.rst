===========================================
HOWTO Setup StorageServers to Log Incidents
===========================================

Get the <LOG_GATHERER.FURL> from monitoring: ``/home/gatherer/incidents/log_gatherer.furl``
actually get the <LOG_GATHERER.FURL> from lae_automation_config.json


On the storageserver-running system:
 (1) ``cd /home/customer/storageserver``
 (2) In section ``[node]`` of ``tahoe.cfg`` add: ``log_gatherer.furl = <LOG_GATHERER.FURL>``
     - be careful to preserve the swissnumber!
