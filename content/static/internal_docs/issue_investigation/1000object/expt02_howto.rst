To run this experiment you need a "Client", and an S3-backed "Storage
Server".  I ran this experiment with the S3-backed Storage Server running on
an AWS EC2, but it should be possible to replicate the experiment with the
Storage Server running on any appropriately configured machine.

==================
Test Client Setup:
==================

.. _follow these instructions: https://tahoe-lafs.org/trac/tahoe-lafs/browser/trunk/docs/quickstart.rst
.. _configuration-instructions: https://tahoe-lafs.org/trac/tahoe-lafs/browser/trunk/docs/configuration.rst#client-configuration

To set up a running instance of Tahoe-LAFS [#]_ `follow these instructions`_.
I refer to the resultant Client instance of Tahoe as a "Gateway", i.e. a
Tahoe node that functions as a portal to other nodes, but does not itself
provide storage.  A typical (and optimally secure) setup is to run the
Gateway as a local service on the machine most proximal to the User, other
configurations are possible...

I assume you successfully configure your Gateway's ``tahoe.cfg`` file, per these
configuration-instructions_.

===================
Test HostEC2 Setup:
===================

I refer to the EC2 used in the experiment as a "HostEC2" because its function is
to host a Tahoe Storage Server. The modifier "Test" distinguishes the EC2
instance from production EC2s, in particular the expected lifespan of this EC2
is approximately equal to the duration of the experiment.

The following assumes that the experimenter will *not* be monitoring in real
time, if you are monitoring in real time, then the ``--quiet`` option is
unnecessary.  You will probably also be interested in the setup described in
`the realtime monitoring howto`_.


Prepare and Access the Test HostEC2:
------------------------------------

.. _these instructions: https://leastauthority.com/signup/

(1) Setup the Host EC2 per `these instructions`_, in particular following the product specific link will result in the launch of a configured Host EC2.

(2) Access the Host EC2 (if you purchased LAE's services we'll need to make some arrangement to provide ssh access):
     ``ssh -i EC2ADMINSSHPRIVKEYFILE.pem ubuntu@PUBIP_OF_TESTEC2`` [#]_

(3) Set background to default state:
     ``sudo apt-get update && sudo apt-get upgrade -y``

(4) Install ``git``:
     ``sudo apt-get install -y git-core``

(5) Restart the server (some upgrades might have necessitated this):
     ``sudo shutdown -r now``

(6) Access the Host EC2, as "customer":
     ``ssh -i EC2ADMINSSHPRIVKEYFILE.pem customer@PUBIP_OF_TESTEC2``

Set the repository to the Control state:
----------------------------------------
 
.. _reverse patch: ./reverse-prefix-change.darcs.patch
.. _the state it was in when I ran this experiment: https://tahoe-lafs.org/trac/tahoe-lafs/browser/ticket999-S3-backend

In the future, of course, the source code will not be in `the state it was in
when I ran this experiment`_.  To reproduce the experiment, the code must be
returned to its state at the time of the experiment.  

Because production Host EC2's have storage servers installed via ``darcs`` (at
the time of this writing), and because ``git`` provides a strong assurance
that the data it manages is in a certain state, it was necessary to replace
the default LAF_source repository.
   
(1) Start in the right spot (all in the context of the Test Host EC2):
     ``cd /home/customer``

(2) Move the default repo out of the way:
     ``mv /home/customer/LAFS_source /home/customer/defaultsourcebackup``

(3) Use ``git`` to obtain the Control Code (note the "--branch control" option is essential):
     ``git clone --branch control https://github.com/zancas/1000objexpt.git /home/customer/LAFS_source``

(4) Build tahoe:
     ``cd /home/customer/LAFS_source/ && python ./setup.py build | tee controlbuildlog_`date +%s```


Prepopulate the S3 Bucket:
--------------------------

.. _makebackupcore.py: ./makebackupcore.py
.. _here: https://leastauthority.com/howtoconfigure

The hypothesis being tested is that an excess of 1000 objects in the S3
bucket is causing the S3 interface to return truncated query responses.  To
test this hypothesis we need a bucket with an excess of 1000 objects in it!

To generate this bucket:

(1) run `makebackupcore.py`_ which generates 2000 files each of which is
slightly larger than 55 bytes, i.e. 56 or more (but not a lot more) bytes, in
a local directory.

(2) Follow the directions `here`_ to upload these files to the grid as
immutables, where "DIRECTORY_TO_BACKUP" is "``filesfilestoprepopulatewith``" as
specified in `makebackupcore.py`_.

NOTE:  You need a running EC2 with the Control source code installed and a Gateway
to Prepopulate using this method, so start at the beginning of this doc!

Set the repository to the Case state:
---------------------------------------

.. _same production version: https://tahoe-lafs.org/trac/tahoe-lafs/browser/ticket999-S3-backend

NOTE:  You probably don't want to set the source code to this (The Case)
state unless you've already run the experiment in the Control state. You
also probably want to prepopulate the S3 Bucket before setting the repository
to this state!

The Control repository contains `the prefix query patch`_, functionality
which ensures that requests for objects from S3 buckets are constrained to a
named subset of all objects in the bucket.  As currently implemented the
prefix query patch specifies a complete storage index, which in the S3 backed
implementation of Tahoe being used by LAE always maps to a single S3 bucket object. 

The Case repository is the `same production version`_ as Control with the
"`reverse patch`_" applied.  This version of the code is referred to by the
"case" branch on the github repository.  The following procedure assumes the
context set in the "*Set the repository to the Control state*" section of
this document.  Specifically it assumes you are in the source directory of a
git repo on a Test Host EC2. To install:

(1) Setup the Case code and clean any leftovers from the Control install:
     ``git checkout --quiet -f case && make clean``

(2) Build tahoe:
     ``cd /home/customer/LAFS_source/ && python ./setup.py build | tee casebuildlog_`date +%s```

(3) Make the Tahoe processes on the Test Host EC2 run the Case code:
     ``cd /home/customer && ./restart.sh``

Prepare the Storage Server (Now Using the Relevant Repository):
---------------------------------------------------------------

Prior to running the test script I removed all incidents from the storage
server's "logs/incidents" directory, so that any incidents recorded there
would be from the test.

I also moved twistd log files out of the way for the same purpose.

(1) Set the running process to use the updated repository (unless you *just* did this):
     ``cd /home/customer/ && ./restart.sh && cd /home/customer/storageserver``

(2) Prep a storage area (codetype is "case" or "control"):
     ``mkdir pre$(codetype)_incidents && mkdir pre$(codetype)_twistdlogs``

(3) Clean the "incidents" directory:
     ``mv logs/incidents/incident* ./pre$(codetype)_incidents``

(4) Clean the twistd logs:
     ``mv logs/twist* ./pre$(codetype)_twistdlogs``

.. _the realtime monitoring howto: ../realtimemonitoring_howto.html


=========================================
Summary of basictahoefileput.py Behavior:
=========================================

.. _basictahoefileput.py: ./basictahoefileput.py 

The test script `basictahoefileput.py`_ generates a directory to hold
run-specific information on each run.  This directory is a child of a
directory named after the relevant state of the source code (i.e. "case" or
"control"), it's name is simply the time (in seconds since the epoch +/- some
drift) of the run.  It contains several files on each run:

(1)  a 'data' file which contains sufficient data to ensure that the resulting LAFS file is not a literal, but rather a 'standard' immutable.

(2)  a time file that contains timing information for each of the (by default 10000) ``put``'s.

     (1) The output of ``time.time()`` immediately before the invocation [#]_ 

     (2) The output of ``time.time()`` immediately after the invocation  

     (3) The difference between the two.

=========================
Post Experiment Analysis:
=========================

.. _incident "flog" files: https://tahoe-lafs.org/trac/tahoe-lafs/browser/trunk/docs/logging.rst

.. _error messages from retries: https://tahoe-lafs.org/trac/tahoe-lafs/browser/ticket999-S3-backend/src/allmydata/storage/backends/s3/s3_common.py#L107


After the each run I filtered the resulting `incident "flog" files`_ to only
include those incidents that occurred during the run as indicated by the
output of `basictahoefileput.py`_.  This was accomplished by running
`timefilter.sh`_.  

* The Control run start was ``1343094921`` the Control run stop was ``1343101930``. 
* The Case run start was ``1343847371`` the Case run stop was ``1343852019``.

Control Run:
------------

.. _this example: ./control_run_webview.html
.. _severityfilter.sh: ./severityfilter.sh
.. _dumper.sh: ./dumper.sh

After viewing one of the time filtered incident "flogfiles". I filtered
incidents (with `severityfilter.sh`_) that occurred with a severity below
25-INFREQUENT.  I did this in part because there were only a few incidents at
or above this level, and in part because I recognized the INFREQUENT level
events as `error messages from retries`_.

Upon inspection (of `dumper.sh`_ dumped flogfiles) I realized that *all* the
INFREQUENT events were retries, and that there was a 1-1 correlation between
those INFREQUENTs and a set of subsequent WEIRDs.  The pattern is illustrated
by `this example`_ html file output from ``flogtool web-view FLOGFILE`` where
FLOGFILE is one of the files that underwent the previously described two
rounds of filtering.

I concluded two things:

(1) Each set of 4 events as in the above example is caused by a single non-response (HTTP 500) error from AWS S3.

(2) Since the immutables were successfully uploaded in this run, these events do not indicate a lack of functionality.

Case Run:
---------

Upon examination of the results of a filtering process similar to that
described above, I found that *all* level 30 events produced during the
experiment were "``truncated get_bucket response``" errors.

That there were 0 such errors (in 10000 puts during the Control run) and 34
out of 1233 trials in the Case run, is strong evidence that the prefix
query patch did indeed resolve issues stemming from requests for too many
objects.

I'm puzzled about the behavior of ``GET`` requests to over-1000 object
buckets though, as I would have expected *all* requests to generate
``truncate get_bucket reponses``'s.


Simulation of Backup [#]_:
--------------------------

.. _issue reported by a user: https://leastauthority.zendesk.com/tickets/5
.. _initial incident error: https://leastauthority.zendesk.com/tickets/5

The `issue reported by a user`_ that stimulated our initial investigation had
slightly different characteristics from the error messages I saw generated
during the Case run.

With Case Repository:
`````````````````````

.. _linked timeout errors: ./backuperroroutput_timeout.txt

To attempt to regenerate the `initial incident error`_ I executed: ``tahoe
backup DIRECTORY ALIAS`` on the "filestoprepopulatewith" directory (see
`makebackupcore.py`_).

I received the `linked timeout errors`_, which I believe should be properly
considered in a separate context, simply included here so they're not
forgotten.  


With The Control Repository:
````````````````````````````

Against Mutables:
-----------------

.. _repeated PUT against a mutable object: ./repeatmutablefileput.py

I noticed that the 410 Gone Error was raised in the context of a mutable
object so I next ran a `repeated PUT against a mutable object`_, in the hopes of
generating the 410 Error.

AM With The Case Repository:
````````````````````````````
Every request (of 24) generated the following error (or similar):

::

 Error: 410 Gone\nUnrecoverableFileError: the directory (or mutable file) could not be retrieved, because there were insufficient good shares. This might indicate that no servers were connected, insufficient servers were connected, the URI was corrupt, or that shares have been lost due to server departure, hard drive failure, or disk corruption. You should perform a filecheck on this object to learn more.

which is in good agreement with the initial error report!

AM With The Control Repository:
```````````````````````````````

In the case of the Control repository there were 0 "``410 Gone``" errors in
the 100 trials I ran!  QED Have a nice day.

.. _timefilter.sh: ./timefilter.sh

.. _the prefix query patch: https://tahoe-lafs.org/trac/tahoe-lafs/changeset/5634/ticket999-S3-backend

.. [#] I git pulled from ``git://github.com/warner/tahoe-lafs`` at 1343089833 which yielded version: "``allmydata-tahoe: 1.9.0.post163``"

.. [#] The IP address was ``23.22.195.217`` for the test I ran.

.. [#] The ``put`` command is executed as a python subprocess.

.. [#] From this point forward I simply toggle code "state"s Case-vs.-Control by running:
    ``cd /home/customer/LAFS_source && make clean && git checkout $(state) && make clean && python ./setup.py build && cd /home/customer && /home/customer/restart.sh``
