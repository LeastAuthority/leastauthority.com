
Problem Statement:
==================

 During some operations, at least HTTP "PUT"s invoked in the course of a
 backup, the gateway receives an error message from the storageserver.  The
 storageserver is transmitting an error it received from the S3 instance.
 The error emitted by the S3 instance "S3Error" is an HTTP response with a
 500 header, and body:

  ``"Error Message: We encountered an internal error. Please try again."``

Research:
=========

 Zooko and David-Sarah performed extensive research documented in `Tahoe-LAFS
 ticket 1590`_.  The `AWS Service Level Agreement`_ says that they intend for
 at least 99% of requests to succeed and less than 1% of requests to incur
 errors like this, and they will pay a discount if they fail to achieve
 that. It looks like we're getting the error on substantially more than 1% of
 requests, but we're still working on collecting statistics in order to
 confirm that. This may indicate some undiagnosed problem on AWS's side.

.. _Tahoe-LAFS ticket 1590: https://tahoe-lafs.org/trac/tahoe-lafs/ticket/1590
.. _AWS Service Level Agreement: https://aws.amazon.com/s3-sla/

Implemented Solution:
=====================

 Pending better diagnosis of the issue, we have deployed a retry-once-patch_ that retries
 requests that receive "HTTP-5xx" -headed (a superset of the "HTTP-500"
 -headed) responses. We retry only one time, and if it fails a second time we
 return the error to the client.

.. _retry-once-patch: https://tahoe-lafs.org/trac/tahoe-lafs/browser/ticket999-S3-backend/src#allmydata

Experimental Goal:
==================

 Determine whether the implemented solution is sufficient to cause up and
 downloads to succeed in the presence of rare HTTP-500 responses.

Procedure:
==========

General:
--------
 
  (1) Turn off storageserver

  (2) Set state of experimental variable, in this case, first without 5xx-retry patch, and then with.

  (3) restart server

  (4) restart and flogtool tail logport
 
AND

Experiment One:
---------------

  (1) Create a directory in the experimental TLoS3 instance.

  (2) Using the "tahoe backup" command upload 101 files from: 0 (inclusive) to: 792 byte in size in 8 byte increments

Experiment Two:
---------------

  (1) Create a directory in the experimental TLoS3 instance.  

  (2) Using the "tahoe backup" command upload 101 to 1001 [in increments of 100] files in 10 successive operations. 
 
    Files are sized in multiples of 8 from 0 to (number of files-1)*8 bytes.  For example: In the 7th batch of files there were 700 files, the smallest was 0 bytes the largest was 699*8 bytes. 

Results:
--------

Experimental (Without 5xx-Retry Patch):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 The HTTP-500 headed errors were recapitulated in Experiment Two only.  This
 is not too surprising given the low-frequency of the 500 responses.  More
 tests with the revised protocol will be run.

Experimental (With 5xx-Retry Patch):
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 The results are ambiguous, I could not determine whether the upload failed
 or my monitoring tool failed.  I should be able to determine this by
 inspecting the state of the TLoS3 store, however rather than investigating
 in this way, I'll run the single mutable files with multiple
 reads-and-writes experiment.  This test with its single generated capability
 will be easier to analyze. (Thanks to David-Sarah for the suggestion!)

Experiment Three:
~~~~~~~~~~~~~~~~~

 In order to simplify the resulting data set, I'll "tahoe put" to the same
 mutable file repeatedly.

Protocol:
---------

 (1) Create a mutable empty file.

 (2) Append a '\n' terminated string at a time to that empty file, by inspection of the file I can then tell when/if "puts" fail. 
 
 (3) Repeat (2) 1000 times.

Control Run:
------------

 When I ran the test with the 5xx-retry-once patch the mutable file was
 successfully updated 1000 times.

 During this run the test script invoking the gateway "put" command emitted
 the following error one time and success 999 times::

  ``On the 109th 'put'. HTTPCODE is Error:. Comm Tuple is ('', 'Error: 500 Internal Server Error\n"Traceback (most recent call last):\\x0aFailure: allmydata.mutable.common.NotEnoughServersError: (\'Publish ran out of good servers, last failure was: [Failure instance: Traceback (failure with no frames): <class \\\\\'foolscap.tokens.RemoteException\\\\\'>: <RemoteException around \\\\\'[CopiedFailure instance: Traceback from remote host -- Traceback (most recent call last):\\\\n  File \\"/usr/local/lib/python2.6/dist-packages/Twisted-11.1.0-py2.6-linux-i686.egg/twisted/internet/tcp.py\\", line 277, in connectionLost\\\\n    protocol.connectionLost(reason)\\\\n  File \\"/usr/local/lib/python2.6/dist-packages/Twisted-11.1.0-py2.6-linux-i686.egg/twisted/web/client.py\\", line 191, in connectionLost\\\\n    self.factory._disconnectedDeferred.callback(None)\\\\n  File \\"/usr/local/lib/python2.6/dist-packages/Twisted-11.1.0-py2.6-linux-i686.egg/twisted/internet/defer.py\\", line 362, in callback\\\\n    self._startRunCallbacks(result)\\\\n  File \\"/usr/local/lib/python2.6/dist-packages/Twisted-11.1.0-py2.6-linux-i686.egg/twisted/internet/defer.py\\", line 458, in _startRunCallbacks\\\\n    self._runCallbacks()\\\\n--- <exception caught here> ---\\\\n  File \\"/usr/local/lib/python2.6/dist-packages/Twisted-11.1.0-py2.6-linux-i686.egg/twisted/internet/defer.py\\", line 545, in _runCallbacks\\\\n    current.result = callback(current.result, *args, **kw)\\\\n  File \\"/home/customer/LAFS_source/src/allmydata/storage/backends/s3/s3_common.py\\", line 98, in <lambda>\\\\n    lambda f2:  _log_and_maybe_reraise(\\"repeated failure: \\", True))\\\\n  File \\"/home/customer/LAFS_source/src/allmydata/storage/backends/s3/s3_common.py\\", line 79, in _log_and_maybe_reraise\\\\n    raise f.value\\\\nallmydata.storage.backends.s3.s3_common.TahoeS3Error: (\\\\\'500\\\\\', \\\\\'500 Internal Server Error\\\\\', \\\\\'<?xml version=\\"1.0\\" encoding=\\"UTF-8\\"?>\\\\\\\\n<Error><Code>InternalError</Code><Message>We encountered an internal error. Please try again.</Message><RequestId>48B583860CB8E0D3</RequestId><HostId>bOmdhKOJpGcM7HzZkhZgN52CP92S7GLECdiCU789VLqxJw6ybvOeos7i63eEBe2F</HostId></Error>\\\\\')\\\\n]\\\\\'>\\\\n]\', None)\\x0a"\n').``


 When I ran it -- again 1000 times -- without the retry-once-patch_, and manually hit C-r in a WUI while it was running, I found three errors. The first and third one appeared in the web browser after C-r. The second one I found in the twistd.log.

Error One:
----------

::
  
  NotEnoughSharesError: This indicates that some servers were unavailable, or that shares have been lost to server departure, hard drive failure, or disk corruption. You should perform a filecheck on this object to learn more.

::

  The full error message is:
  ran out of servers: have 0 of 1 segments found 1 bad shares encoding 1-of-1, last failure: [Failure instance: Traceback: <class 'allmydata.mutable.common.CorruptShareError'>: <CorruptShareError server=q2z53drs shnum[0]: block hash tree failure: new hash 6tfuzsou6r24nresydn2caelrbgc5paq62pehedut7pkpm4kh6qq does not match existing hash kjv45it5yr4xhlwsyid56g4wdapaxzab2bczvrve4fg3kqgyapcq at [0 of 1] (leaf [0] of 1)
  /usr/lib/python2.7/dist-packages/twisted/internet/defer.py:542:_runCallbacks
  /usr/lib/python2.7/dist-packages/twisted/internet/defer.py:791:_cbDeferred
  /usr/lib/python2.7/dist-packages/twisted/internet/defer.py:361:callback
  /usr/lib/python2.7/dist-packages/twisted/internet/defer.py:455:_startRunCallbacks
  --- <exception caught here> ---
  /usr/lib/python2.7/dist-packages/twisted/internet/defer.py:542:_runCallbacks
  /home/arc/tahoe-lafs/src/allmydata/mutable/retrieve.py:798:_validate_block
  ]


Error Two:
----------

::
  
  foolscap.tokens.Violation: Violation (<RootSlicer>.<call-325-1-msg>.<arg[0]>.??): ('cannot serialize (\'500\', \'500 Internal Server Error\', \'<?xml version="1.0" encoding="UTF-8"?>\\n<Error><Code>InternalError</Code><Message>We encountered an internal error. Please try again.</Message><RequestId>247FBA9DC8257E60</RequestId><HostId>3bZpSskME8kqAiHLNjxNF1rILun46GNwqCEjZzzJZvW0zzpMWM0H1VgUS8Lv2Hk+</HostId></Error>\') (<class \'allmydata.storage.backends.s3.s3_common.TahoeS3Error\'>)',)
  ]

Error Three:
------------

::
  
  
  UnrecoverableFileError: the directory (or mutable file) could not be retrieved, because there were insufficient good shares. This might indicate that no servers were connected, insufficient servers were connected, the URI was corrupt, or that shares have been lost due to server departure, hard drive failure, or disk corruption. You should perform a filecheck on this object to learn more.
 

 I double check here, to ensure that I am indeed running the unpatched
 version of tahoe: ::

  customer@ip-10-116-173-5:~$ ./restart.sh 
  STOPPING '/home/customer/introducer'
  process 19533 is dead
  STARTING '/home/customer/introducer'
  STOPPING '/home/customer/storageserver'
  process 19545 is dead
  STARTING '/home/customer/storageserver'
  customer@ip-10-116-173-5:~$ ls
  ctab  introducer  LAFS_source  restart.sh  storageserver
  customer@ip-10-116-173-5:~$ cd storageserver/
  customer@ip-10-116-173-5:~/storageserver$ ls -l
  total 60
  -rw-r--r-- 1 customer customer     6 2012-05-08 01:33 client.port
  -rw-r--r-- 1 customer customer   156 2012-04-03 17:08 data
  -rw-r--r-- 1 customer customer 11577 2012-04-27 20:53 flogdump.txt
  drwxr-xr-x 3 customer customer  4096 2012-05-07 02:57 logs
  -rw-r--r-- 1 customer customer    33 2012-05-08 01:33 my_nodeid
  -rw------- 1 customer customer    23 2012-05-08 01:33 node.url
  drwx------ 2 customer customer  4096 2012-05-08 01:33 private
  -rw-r--r-- 1 customer customer  1183 2012-04-09 15:38 reallyoldconf
  drwxr-xr-x 3 customer customer  4096 2012-04-26 23:10 storage
  -rw-r--r-- 1 customer customer   644 2012-04-09 16:19 tahoe.cfg
  -rw-r--r-- 1 customer customer   291 2012-02-04 03:40 tahoe-client.tac
  drwxr-xr-x 2 customer customer  4096 2012-02-04 03:40 tmp
  -rw-r--r-- 1 customer customer     5 2012-05-08 01:33 twistd.pid
  customer@ip-10-116-173-5:~/storageserver$ cat my_nodeid 
  q2z53drszkkotmp6lkxetp2pvbd5rhnd
  customer@ip-10-116-173-5:~/storageserver$ dirs
  ~/storageserver
  customer@ip-10-116-173-5:~/storageserver$ pushd /home/customer/LAFS_source/
  ~/LAFS_source ~/storageserver
  customer@ip-10-116-173-5:~/LAFS_source$ darcs pull
  Pulling from "https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend"...
  Fri Mar  9 05:29:38 UTC 2012  david-sarah@jacaranda.org
    * S3 backend: retry 5xx errors once.
    Shall I pull this patch? (1/4)  [ynWsfvplxdaqjk], or ? for help: n
    Skipped pull of 3 patches.
    You don't want to pull any patches, and that's fine with me!
  customer@ip-10-116-173-5:~/LAFS_source$ 


  There are 133 Errors in the no-retry patch log. 63 are ``Error 410
  Gone``'s, 69 are ``Error: 500 Internal Server Error``'s, and the last one
  was the following:

::

  At 1336459086.09, on the 4644th 'put' HTTPCODE is:  Traceback .
  Comm Tuple is ('', 'Traceback (most recent call last):\n  File "/home/arc/tahoe-lafs/support/bin/tahoe", line 9, in <module>\n    load_entry_point(\'allmydata-tahoe==1.9.0.post132\', \'console_scripts\', \'tahoe\')()\n  File "/home/arc/tahoe-lafs/src/allmydata/scripts/runner.py", line 116, in run\n    rc = runner(sys.argv[1:], install_node_control=install_node_control)\n  File "/home/arc/tahoe-lafs/src/allmydata/scripts/runner.py", line 102, in runner\n    rc = cli.dispatch[command](so)\n  File "/home/arc/tahoe-lafs/src/allmydata/scripts/cli.py", line 541, in put\n    rc = tahoe_put.put(options)\n  File "/home/arc/tahoe-lafs/src/allmydata/scripts/tahoe_put.py", line 85, in put\n    resp = do_http("PUT", url, infileobj)\n  File "/home/arc/tahoe-lafs/src/allmydata/scripts/common_http.py", line 68, in do_http\n    return c.getresponse()\n  File "/usr/lib/python2.7/httplib.py", line 1027, in getresponse\n    response.begin()\n  File "/usr/lib/python2.7/httplib.py", line 407, in begin\n    version, status, reason = self._read_status()\n  File "/usr/lib/python2.7/httplib.py", line 371, in _read_status\n    raise BadStatusLine(line)\nhttplib.BadStatusLine: \'\'\n').
