

===================================
issue 3 -- can't upload large files
===================================

symptoms
========

Commands such as "tahoe backup" go excessively slow or fail if a file being
backed up is larger than about 150 MB.


diagnosis
=========

The current Tahoe-LAFS storage server code (which is called the "S3 Backend"
in the Tahoe-LAFS source code repositories) holds the entirety of the
ciphertext of a file in RAM while uploading it to S3. Therefore if the size
of the file (combined with various other things that affect RAM usage)
overload the storage server's total physical RAM, the upload will take
excessively long or will fail. Empirically, uploading files larger than about
150 MB is prone to problems.

fix -- not yet deployed
=======================

The DARPA-funded new storage server code (which is called the "Cloud
Backend") fixes this limitation for immutable files, by uploading the file in
a series of reasonably-sized chunks. With that code, immutable files of any
size can be uploaded.

The Cloud Backend has not yet been deployed to LAE customer storage
servers. When we do deploy it we'll update this issue report.

further issue -- large mutable files
====================================

Even the Cloud Backend, which processes immutable objects in a streaming
fashion, processes mutable objects all at once. Therefore large mutable
objects still will not work. Typical usage of Tahoe-LAFS (e.g. using the
"tahoe backup" command) uses immutable files for everything except for some
directories, and directories are usually not that large, so under such usage
the limitation of the size of mutables will not interfere.

Fixing this is somewhat involved, for compatibility reasons and because if
the upload were to be interrupted, the partially uploaded file could be left
corrupted. In the future, we intend (with the other Tahoe-LAFS hackers) to
develop an improvement to Tahoe-LAFS involving an end-to-end
two-phase-commit.
