Experiment One:
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
