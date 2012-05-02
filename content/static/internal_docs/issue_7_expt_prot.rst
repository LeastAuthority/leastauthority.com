

Problem Statement:
==================

 During some operations, at least HTTP "PUT"s invoked in the course of a
 backup, the Gateway receives an error message from the storageserver.  The
 storageserver is transmitting an error it received from the S3 instance.
 The error emitted by the S3 instance "S3Error" is an HTTP response with a
 500 header, and body:

``"Error Message: We encountered an internal error. Please try again."``

Research:
=========

 Zooko and David-Sarah performed extensive research documented on `Tahoe-LAFS
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

 Pending better diagnosis of the issue, we have deployed a patch that retries
 requests that receive "HTTP-5xx" -headed (a superset of the "HTTP-500"
 -headed) responses. We retry only one time, and if it fails a second time we
 return the error to the client.

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
