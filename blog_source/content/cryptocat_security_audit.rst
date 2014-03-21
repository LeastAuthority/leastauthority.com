.. -*- coding: utf-8-with-signature-unix; fill-column: 73; indent-tabs-mode: nil -*-

Least Authority Performs Security Audit For CryptoCat
=====================================================

:date: 2014-03-20 01:00
:tags: consultancy, cryptocat
:category: News
:slug: least_authority_performs_security_audit_for_cryptocat
:author: Zooko Wilcox-O'Hearn
:summary: Least Authority performs security audit for CryptoCat

This is the second post in our series about security audits of Free and
Open Source end-to-end encryption software. The first post in the series
was about our security audit of SpiderOak's `crypton`_ project.

`Our mission`_ at LeastAuthority is to bring verifiable end-to-end
security to everyone.

As part of that mission, in addition to operating the `S4`_ simple secure
storage service, we also perform security consulting. We
LeastAuthoritarians have extensive experience in security and
cryptography, and other companies hire us to analyze the security of their
protocols and software.

We audited the widely-used `CryptoCat`_ encrypted chat program. This
audit was funded by `Open Technology Fund`_ as part of their `Red Team`_
project to provide multiple professional security audits to Internet
freedom projects.

What were the results?
----------------------

We found several security issues in the version of CryptoCat that we
examined (CryptoCat v2.1.15). For each one, we reported it to the
CryptoCat developers, and they have either deployed a fix in a newer
release of CryptoCat or else disabled the feature that has the
vulnerability.

The complete list of the issues we found is at the end of this article,
along with a link to the report document.

Unfortunately we didn't have time to examine all parts of CryptoCat that
we wanted to. We concentrated on the “crypto-related” parts: key
generation and key management, random number generation, encryption and
decryption, authentication and integrity, and the new file transfer
feature. Most of the issues that we found were in those areas.

Our report explains what parts of it we looked at most closely (this is
called the "coverage" results of the audit).

parting thoughts
----------------

I would like to thank the CryptoCat project, led by Nadim Kobeissi, for
their commitment to doing development in the open, inviting external
review, and moving to address the issues we uncovered. This open
development process is a good complement to CryptoCat's Free and Open
Source publication of their code and their commitment to providing
end-to-end security for their users.

On top of all of the above, I'd like to thank CryptoCat for their
unflagging focus on *usability*. Usability is a critical factor if we are
going to succeed at bringing verifiable end-to-end security to everyone,
and it is an area where we as a community and as a society need to
improve.

any questions?
--------------

If you have any questions about these results or the process, the
CryptoCat developers and we of LeastAuthority.com will be available on a
reddit "Ask Me Anything" Monday of next week for Q&A.

The next project we are auditing is `GlobaLeaks`_, so stay tuned.

XXX list issues
XXX link to report

.. _crypton: /blog/least_authority_performs_security_audit_for_spideroak.html
.. _Our mission: https://leastauthority.com/about_us
.. _CryptoCat: https://crypto.cat/
.. _Open Technology Fund: https://www.opentechfund.org/
.. _Red Team: https://www.opentechfund.org/labs#redteam
.. _GlobaLeaks: https://globaleaks.org/

.. _S4: https://leastauthority.com/product_s4
.. _Our mission: https://leastauthority.com/about_us
.. _SpiderOak: https://spideroak.com/
.. _Crypton.io: https://crypton.io/
.. _has published the security auditing report: https://spideroak.com/blog/20140220090004-responsibly-bringing-new-cryptography-product-market
.. _Cryptocat: https://crypto.cat/
