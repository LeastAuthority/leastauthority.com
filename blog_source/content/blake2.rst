.. -*- coding: utf-8-with-signature-unix; fill-column: 73;
.. -*- indent-tabs-mode: nil -*-

BLAKE2: “Harder, Better, Faster, Stronger” Than MD5
===================================================

:date: 2014-03-21 22:00
:tags: cryptography, engineering
:category: Essay
:slug: BLAKE2-harder-better-faster-stronger-than-MD5
:author: Zooko Wilcox-O'Hearn
:summary: BLAKE2: “Harder, Better, Faster, Stronger” Than MD5

*best read while listening to Daft Punk:* `Harder, Better, Faster, Stronger`_

.. _`Harder, Better, Faster, Stronger`: https://youtu.be/gAjR4_CbPpQ

----

Why use `BLAKE2`_ instead of Skein, Keccak (SHA-3), MD5, or SHA-1 as a secure
hash function?

BLAKE was the best-rated hash function in the SHA-3 competition
---------------------------------------------------------------

NIST, in `the final report of the SHA-3 competition`_, said this about the
finalists (which included BLAKE, Keccak, Skein, and Grøstl):

* BLAKE had a security margin — the gap between a known-weak reduced version
  and the full version — comparable to Keccak and superior to the other
  finalists. (§4.3: “BLAKE and Keccak have very large security margins.”)

* BLAKE had a depth of analysis — the amount of published research analyzing
  its security — comparable to Grøstl and Skein and superior to the other
  finalists. (§3.1: “Keccak received a significant amount of cryptanalysis,
  although not quite the depth of analysis applied to BLAKE, Grøstl, or
  Skein”)

* BLAKE had performance (in software) comparable to Skein and superior to the
  other finalists. (§5.1.4: “Skein and BLAKE […] have the best overall
  software performance.”)

.. _the final report of the SHA-3 competition: http://nvlpubs.nist.gov/nistpubs/ir/2012/NIST.IR.7896.pdf

but BLAKE was similar to SHA-2
------------------------------

So if BLAKE1 was in the top tier in all three of these measures, why didn't
NIST choose BLAKE1 to be the winner of the SHA-3 contest? The main reason is
given in §3.4 of the final report: because BLAKE1's design was similar to
SHA-2's.

When the SHA-3 project was announced, being like SHA-2 was explicitly listed
as an undesirable property. That made sense at the time, but today, being
like SHA-2 should *increase* your confidence in a hash function's
security. Here's why:

When the SHA-3 project was announced (in 2007), `MD5`_ and (to a lesser
extent) `SHA-1`_ had just been shockingly revealed to be weak, by a
previously-unknown cryptographer from China, Xiaoyun Wang. There was a
general fear among cryptographers that SHA-2 might be next.  SHA-2's design
is like that of SHA-1 and MD5. SHA-2 was still relatively new (having been
published in 2002) and was not yet widely used compared to MD5 or SHA-1. This
was actually the impetus for launching the SHA-3 competition: to have a new
hash function ready in case SHA-2 was suddenly shown to be unsafe. At the
same time, NIST advised everyone to transition from MD5 and SHA-1 to SHA-2
immediately, instead of waiting for the eventual standardization of SHA-3.

This explains why it was a design criterion for SHA-3 candidates to be
*different* from SHA-2: because the purpose of SHA-3 was to be available as a
fallback in case SHA-2 failed!

.. _MD5: http://eprint.iacr.org/2004/199.pdf
.. _SHA-1: http://people.csail.mit.edu/yiqun/SHA1AttackProceedingVersion.pdf

but being similar to SHA-2 is good!
-----------------------------------

Now, however, another seven years have gone by, and further efforts by
cryptographers to analyze SHA-2 have not found any way to defeat it.  This
means that SHA-2 is now twelve years old, and during most of that time it has
been the most widely recommended secure hash function in the world. So today,
the fact that BLAKE has a few design elements in common with SHA-2 doesn't
seem to reflect badly on BLAKE at all.

.. This would be a perfect place to be able to link to your tables of comparison, thereby giving context to "twelve years".
   I would be happy to help you with that post, too. —Am

BLAKE1 compares well to the modern hash functions Keccak and Skein.  There is
good reason to think that it is secure, and it has better performance (in
software, on Intel or ARM CPUs) than Keccak. However, the other two are also
good—there is no reason to suspect any of them of any weakness.

BLAKE2 is faster than MD5
-------------------------

Okay, so what is *BLAKE2* then? Well, after NIST settled on Keccak to be the
winner of the SHA-3 contest, Jean-Philippe Aumasson, Samuel Neves, Christian
Winnerlein, and I decided that what the world needed was not just a secure
hash function that was faster than Keccak, but one that was faster than MD5!
This is because MD5 (and SHA-1) continue to be very widely used, even in new
applications, even though MD5 and SHA-1 are unsafe for many uses. We
hypothesized that offering engineers a hash function that was *both* faster
and more secure than their beloved MD5 or SHA-1 might be more successful than
haranguing them to upgrade to an alternative that is more secure but slower.

So, we took BLAKE1 (Jean-Philippe Aumasson had been one of the designers of
BLAKE1), traded-off a little of its generous security margin in return for
more efficiency, and optimized it to produce *BLAKE2*, which is faster than
MD5 (on a modern Intel CPU). On top of that, we added an optional parallel
mode so that if you have 4 or 8 CPU cores available you can run your BLAKE2
function almost 4 or 8 times as fast.

Bottom line:

* MD5 and SHA-1 are not responsible choices for a secure hash function today
  [*]_.

* Keccak (SHA-3), Skein, and BLAKE2 are all reasonable choices.

* BLAKE2 is not only faster than the other good hash functions, it is even
  faster than MD5 or SHA-1 (on modern Intel CPUs).

Further reading:

Here are `the slides from a presentation that I gave`_ about BLAKE2 at
“Applied Cryptography and Network Security 2013”.

Here is `an essay I posted`_ in April 13, 2012 and updated in October 3,
2012, which outlines the motivation for what later became BLAKE2.

.. _`enough computation to generate SHA-1 collisions`: http://bitcoin.sipa.be/

.. [*] Some software, notably `git`_, is still using SHA-1, and relying on
       the fact that the best publicly-known method of generating SHA-1
       collisions costs 2⁶⁹ computations, which is expensive. I think it is
       unwise to rely on this for two reasons. One is that there could be
       more efficient techniques to compute SHA-1 collisions that we don't
       know about. Another is that the cost of doing 2⁶⁹ computations is
       falling rapidly—at the time of this writing (March 22, 2014), the
       Bitcoin network is performing `enough computation to generate SHA-1
       collisions`_ every 131 minutes!

.. _git: http://www.git-scm.com/

P.S. this isn't about hashing passwords
---------------------------------------

P.S. Secure hash functions are not for hashing passwords! Secure hash
functions are building blocks in cryptographic protocols and they should be
as efficient as possible while still being secure.  *Password-hashing
functions* are for impeding brute force guessing of passwords, and they
should be as inefficient as possible while still being usable. See "scrypt"
and "bcrypt" for current password-hashing functions, and see the `Password
Hashing Competition`_ for some candidate next-generation ones.

By the way, some of the entrants in the Password Hashing Competition use
BLAKE2 as an internal building block in their algorithm. They presumably
chose it because it is fast, and then their design forces the computer to
calculate BLAKE2 many, many times, iteratively, in order to be slow
again. This actually makes sense. ☺

Acknowledgments: Thanks to an anonymous reviewer, Jean-Philippe Aumasson,
Daira Hopwood, and Amber Wilcox-O'Hearn for comments on earlier drafts of
this post. I'm solely responsible for any errors.

.. _BLAKE2: https://blake2.net
.. _Password Hashing Competition: https://en.wikipedia.org/wiki/Password_Hashing_Competition
.. _the slides from a presentation that I gave: https://blake2.net/acns/slides.html
.. _an essay I posted: https://plus.google.com/108313527900507320366/posts/4ZPRdvpzBTJ
