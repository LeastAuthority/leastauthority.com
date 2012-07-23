Test HostEC2 Setup:
===================

.. _the realtime monitoring howto: ../realtimemonitoring_howto.html

The following assumes that the experimenter will not be monitoring in real
time. If you want to do that, then the ``--all`` and ``--quiet`` options are
unnecessary.  You will probably also be interested in the setup described in `the realtime monitoring howto`_.
 
(1) ``ssh -i EC2ADMINSSHPRIVKEYFILE.pem customer@PUBIP_OF_TESTEC2`` [1]_ 

(2) ``cd /home/customer/LAFS_source && darcs pull --quiet --all && darcs obliterate --quiet --all -p'S3 backend: retry 5xx errors once.'`` [2]_

(3) ``cd /home/customer/ && ./restart.sh``

Test Client Setup:
==================

.. _repeatmutablefileput.py: ./repeatmutablefileput.py
.. _Tahoe-LAFS: https://tahoe-lafs.org/trac/tahoe-lafs/browser/trunk/docs/quickstart.rst
.. _configuration-instructions: https://tahoe-lafs.org/trac/tahoe-lafs/browser/trunk/docs/configuration.rst#client-configuration

I assume you have a running instance of Tahoe-LAFS_.

I assume you can configure your gateway's ``tahoe.cfg`` file, per the
configuration-instructions_.

Once your LAFS gateway is introduced to the test HostEC2, ( you must set your
gateway's ``introducer.furl`` to be the same introducer as that referenced
by the test storage server ) the trial is run by simply invoking
repeatmutablefileput.py_.

The test script repeatmutablefileput.py_ generates two files per run, a file
which is iteratively appended to and ``put`` to the test server, and a log
file which is named ```time.time()`.txt``.

Summary of ``repeatmutablefileput.py`` Behavior:
------------------------------------------------

(#) Create an empty mutable file.

(#) Append an '\\n' terminated string at a time to that empty file.

(#) ``tahoe put THEFILE AN_SDMF_WRITECAP``, and log the http response
 
(#) Repeat (3) 10000 times.


.. [#] The IP address was 174.129.177.99 for the test I ran.

.. [#] In the future, of course, the repo will not be in the state it was in when I ran this experiment.  To reproduce exactly one must determine the state of the historical repository.
