To set the repository to the historical "Control" state\ [#]_:
--------------------------------------------------------------
   
(1) Start in the right spot:
     ``cd /home/customer/LAFS_source``

(3) Get the base repo:
     ``darcs pull --quiet --all``

(4) Preserve the 5xx multiple retry patch:
     ``darcs diff -u -p 'Allow retrying a cloud container operation more than once' > multiretry.diff``

(5) Set the repository to the snapshot of the historical "Control" state:
     ``darcs obliterate --quiet --all --from-patch='fix and test the limitation on number of'`` [#]_

(6) Apply the multiple retry patch:
     ``patch -p1 -i multiretry.diff``

(7) The last hunk in s3/s3_common.py will fail to apply, so manually replace lines starting after ``class TahoeS3Error`` with the corresponding lines from:

     https://tahoe-lafs.org/trac/tahoe-lafs/browser/ticket999-S3-backend/src/allmydata/storage/backends/s3/s3_common.py?rev=5647#L67

To set the repository to the "Experimental" state:
--------------------------------------------------

(1) Follow steps (1)-(5), above NOTE: I'm assuming a "Production Repository" for step (1) NOT A "Control" Repository!.
 
(2) Set the repository to the snapshot Control+\ `the prefix query patch`_, Experimental state:
     ``darcs pull --quiet --all -p 'fix and test the limitation on number of'``

(3) Follow steps (6)-(7), above.

.. _the prefix query patch: https://tahoe-lafs.org/trac/tahoe-lafs/changeset/5634/ticket999-S3-backend

.. [#] In the future, of course, the repo will not be in the state it was in
       when I ran this experiment.  To reproduce the experiment, the code base 
       must be returned to the state at the time of the experiment.  I call 
       this the "historical" state. Once the historical state is set, there 
       are two versions of the code that I tested:  The one with `the prefix query
       patch`_ which I call "Experimental" and one without which I call
       "Control".  The obliterate returns the repo to the historical state
       prior to `the prefix query patch`_ 's application, so to the "historical
       Control" state.

.. [#] Use ``darcs pull --dry-run`` to check the state of the repository.
