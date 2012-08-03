

=====================================
issue 1 -- 500 internal error from S3
=====================================

symptoms
========

Commands such as "tahoe backup" would fail and emit one of several possible
error messages such as "We encountered an internal error. Please try again."

diagnosis
=========

On investigation, we learned that some requests from the Tahoe-LAFS storage
servers to the Amazon Simple Storage Service ("S3") were failing with a "500
Internal Error" from S3. This resulted in Tahoe-LAFS operations such as
"tahoe backup" failing.

We ran some tests by sending 10,000 requests in a row to S3. We got 132
errors back from S3 [*]. That's about 1.3% of requests resulting in
errors. That's substantially worse than Amazon's Service Level Agreement --
"SLA" -- in which they say that they'll try to make no more than 0.1% of
requests get errors, and they'll really *really* try to make no more than 1%
of requests get errors. So we've opened a support ticket with them asking
them to investigate (case ID within Amazon Web Services: 59460191).

We implemented retry logic in the Tahoe-LAFS storage servers to retry the
same request again one time if it gets an error. Doing the 10,000-request
experiment with the one-retry in place resulted in 9 requests that got an
error.

We then wrote a patch to retry several times with increasing delays between
tries. This time the 10,000-request experiment showed 0 failures. That's what
we like to see!

We measured the round-trip times in that last experiment. Here is a graph
showing each of the 10,000 requests, how long it took for the request to be
satisfied. Red dots indicate that the request had to be retried at least
once. The green triangle indicates the worst -- the longest round-trip time
-- request out of the 10,000 requests.

.. figure:: rttimes.png
   :width: 800px
   :figwidth: image

   round trip times of 10,000 successive requests

fix
===

We deployed the multi-retry patch to all customer storage servers. If you
were having failures using the Tahoe-LAFS-on-S3 service due to this problem,
those failures are probably.  Please try again and let us know (by emailing
support@leastauthority.com) if it works for you now.

lab notebook
============

For complete details, see the `issue 1 lab notebook`_ that Zancas kept when
performing this investigation.

.. _issue 1 lab notebook: issue_investigation/5xx_retry/expt01_howto.rst
