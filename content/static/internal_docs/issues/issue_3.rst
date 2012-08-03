

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

Even with the Cloud Backend, large mutable files pose a problem because if
the upload were to be interrupted, the partially uploaded file could be left
corrupted. A future extension of Tahoe-LAFS itself to do over-the-network
two-phase commit could solve that problem. In the meantime, even with the
Cloud Backend, you cannot upload mutable files larger than the available RAM
on the server.

The Cloud Backend has not yet been deployed to LAE customer storage
servers. When we do deploy it we'll update this issue report.
