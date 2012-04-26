.. _1590: https://tahoe-lafs.org/trac/tahoe-lafs/ticket/1590

Problem Statement:
==================

 During some operations, at least HTTP "PUT"s invoked in the course of a
 backup (a many iteration process), the Gateway receives an error message
 from the storageserver.  The storageserver is transmitting an error it
 received from the S3 instance.  The error emitted by the S3 instance
 "S3Error" is an HTTP packet with a 500 header, and body:

``"Error Message: We encountered an internal error. Please try again."``

Research:
=========

 Zooko performed extensive research documented on Tahoe-LAFS ticket 1590_.
 If I understand correctly he discovered that the kind of error we're seeing
 is common enough that AWS expected to tolerate it in up to 1 of 5000 HTTP
 requests.

Implemented Solution:
=====================

 Acknowledging that more elegant solutions are available we are, for the time
 being at least, retrying requests that receive "HTTP-5xx" -headed (a strict
 superset of the "HTTP-500" -headed) responses a single time.

Experimental Goal:
==================

 Determine whether the implemented solution is sufficient to cause up and
 downloads to succeed in the presence of rare HTTP-500 packets.

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

  (1) Create a directory in the experimental TLOS3 instance.

  (2) Using the "tahoe backup" command upload 101 files from: 0 (inclusive) to: 792 byte in size in 8 byte increments

Experiment Two:
---------------

  (1) Create a directory in the experimental TLOS3 instance.  

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
 inspecting the state of the TLOS3 store, however rather than investigating
 in this way, I'll run the single mutable files with multiple
 reads-and-writes experiment.  This test with its single generated capability
 will be easier to analyze. (Thanks to David-Sarah for the suggestion!)
