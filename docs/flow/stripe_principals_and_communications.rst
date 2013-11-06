.. _Stripe: https://stripe.com
.. _The JS loaded from Stripe: https://stripe.com/docs/stripe.js
.. _The Python Library Installed on the LA Webserver: https://github.com/stripe/stripe-python
.. _root resource: https://twistedmatrix.com/documents/current/web/howto/web-in-60/static-dispatch.html
.. _Our interface to the Web: https://github.com/LeastAuthority/leastauthority.com
.. _/subscribing: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/handlers/subscribing.py
.. _Twisted Web Resource:
.. https://twistedmatrix.com/documents/current/api/twisted.web.resource.Resource.html
.. _/subscription_complete: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/handlers/subscription_complete.py
.. _Aurora: http://ppa.launchpad.net/ubuntu-mozilla-daily/firefox-aurora/ubuntu/
.. _their API: https://stripe.com/docs/api

Introduction
============

 During Stripe_ -processed signup for Our services several 
 Principal Actors send Messages to each other.  This document describes the 
 expected behavior of these "Principals" and Messages in each of several cases. 

 With respect to the behaviors within its scope, it is intended to be
 authoritative (i.e. look here first!), comprehensive (let's not leave any
 relevant part out!), and clear (this doc should reify our common
 understanding). In short, this is the place to explain how it works. 

 A note on the format:
   Each case is intended to contain a clear sequence of behaviors the reader
   can engage in, in order to (re)produce the case.

Definitions
-----------

  LA Webserver
    `Our interface to the Web`_

  Principals Actors (aka Principals)
    Entities that send and receive Messages during the signup process.

    - `Twisted Web Resource`_ s, Provided By the LA Webserver

       - `/subscribing`_
       - `/subscription_complete`_
  
    - The User's Browser (I usually use `Aurora`_, so it's the most tested)
    - Stripe Servers via `their API`_
   
  Message
    Data that is processed by two or more Principals, and is (approximately) invariant 'in between'

  The System
    All Principals and Messages

  The Start Point
    Each case begins with a browser which has loaded the content provided in
    response to an HTTP GET request against the "`root resource`_" at:

      ``https://leastauthority.com``

    The User is provided with a "button class" HTML Anchor Element, labeled
    "Signup", which on-click causes the browser to request via HTTP GET the
    resource provided by "`/subscribing`_".  

    I define the state of The System immediately after the browser has
    successfully loaded this content as "The Start Point". 

    This analysis is focused on the behavior that occurs between this Start
    Point and the subsequent End Point.

  The End Point
    The state of The System that is reached when all Prinicipals have
    finished processing Messages from other Principals.

    This definition implies that non-Principal behavior is not within the scope of the
    analysis, even if it is 'ultimately' caused by a Principal Message.  In
    particular, this document is not concerned with the behavior of the
    flappserver, or other components of the webserver that is stimulated
    by Principal-generated Messages. 

  Service
    The product we are providing for a fee.

  Stripe Servers
    The agency managing credit card payments processing, verification, and
    billing.
 
  Stripe Python Library
    `The Python Library Installed on the LA Webserver`_ (via pip)

  Stripe JS
    `The JS loaded from Stripe`_ into the User's browser

  User
    The agent controlling the User's Browser

  The Signup Process
    The behaviors that occur between the Start- and End- Points specified below.


Caveat Emptor
-------------

 This is a Message-Centric analysis.  That seemed to be a reasonable
 framework for carving up the system.  I make no claim that it's in-any-sense optimal.

In All Cases
------------

  1. Start Your Local Test Server With the Stripe Signup Implementation

     - ``cd YOUR_leastauthority.com_REPO``
     - ``git checkout 103_implement_stripe_01``
     - ``git pull git@github.com:LeastAuthority/leastauthority.com.git``
     - ``cd YOURLOCALPATH/leastauthority.com ./runsite --dev``

  2. Navigate to the test site

     - point your favorite browser (Firefox) at: 

       ``http://localhost:8000``

     - open the browser's webconsole (Ctrl-Shift-K)
     - deselect CSS, JS, Security, and Logging tabs

  3. Navigate to The Start Point

     - click the "Sign up!" button, and get 'redirected' to:

       ``http://localhost:8000/subscribing``

Case One: Valid CC All Nodes Available
--------------------------------------

Here's a set of valid form input data:

 Email address:
   test@test
 Name:
   testvalidCC_01
 Card number:
   4242 4242 4242 4242
 CVC:
   111
 Expiration:
   01 / 2015
