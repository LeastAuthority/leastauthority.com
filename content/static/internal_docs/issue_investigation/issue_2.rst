

================================================
issue 2 -- failure to store more than 1000 files
================================================

symptoms
========

Commands such as "tahoe backup" would fail and emit an error message such as
"410 Gone UnrecoverableFileError".


diagnosis
=========

We realized that this happens when the user has stored more than 1000
files. The Amazon Simple Storage Service ("S3") API for listing the contents
of an S3 bucket returns no more than 1000 object names at a time (with a
marker indicating whether there were more than 1000 objects and the listing
of 1000 that it is giving you is incomplete).

To make sure we understood the behavior, we ran some tests by creating a test
account, uploading more than 1000 files to it, and confirming that using
Tahoe-LAFS on it failed in the same way without our patch, and succeeded with
our patch. We made sure that the same was true when using mutable files and
immutable files.

We ran the same experiment several thousand times with different settings and
there were no anomalies.

We also double-checked the log messages generated in both cases to make sure
none of them were evidence of any other problem.

fix
===

We wrote and deployed a patch which detects this condition and handles
it. With the patch, the Tahoe-LAFS storage server sends a series of requests,
each of which yields another 1000-object-name part of the complete list, and
collects all of the results.

We also opened `a ticket`_ on the Tahoe-LAFS trac to inform other Tahoe-LAFS
developers and users about the issue and the patch to fix it.

If you were having failures using the Tahoe-LAFS-on-S3 service due to this
problem, then those failures should now be fixed. Please try again and let us
know (by emailing support@leastauthority.com) if it works for you now.

.. _a ticket: https://tahoe-lafs.org/trac/tahoe-lafs/ticket/1678

lab notebook
============

For complete details, see the `issue 2 lab notebook`_ that Zancas Wilcox kept
when performing this investigation.

.. _issue 2 lab notebook: 1000object/
