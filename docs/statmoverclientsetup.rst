Obtaining a Statmover Client:
=============================

Get a tarball from statmover somehow.
Put it in someplace on someserver (almost certainly theta).
Check its signature against some key.

Two components of the statmover User-system:
--------------------------------------------

 (1) The metric:
 (2) The emitter:

Deploying the Statmover Client to Storage Server Hosting EC2s:
==============================================================

SSEC2 = "Storage Server hosting EC2 instance"

(1) ``scp -i <PRIVKEY> statmover-lae-<TIMSTAMP>.tar.gz monitor@<EC2 PUBIP>:``
(2) On SSEC2 as ``monitor``: ``cd && tar -xzvf statmover-lae-<TIMSTAMP>.tar.gz``
(3) Follow directions in statmover package README.txt.
