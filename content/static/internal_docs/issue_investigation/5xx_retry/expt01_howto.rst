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
