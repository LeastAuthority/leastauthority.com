.. -*- coding: utf-8-with-signature -*-
.. _Stripe: https://stripe.com
.. _The JS loaded from Stripe: https://stripe.com/docs/stripe.js
.. _The Python Library Installed on the LA Webserver: https://github.com/stripe/stripe-python
.. _root resource: https://twistedmatrix.com/documents/current/web/howto/web-in-60/static-dispatch.html
.. _Our interface to the Web: https://github.com/LeastAuthority/leastauthority.com
.. _/subscribing: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/handlers/subscribing.py
.. _Twisted Web Resource:
.. https://twistedmatrix.com/documents/current/api/twisted.web.resource.Resource.html
.. _/subscription-complete: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/handlers/subscription_complete.py
.. _Aurora: http://ppa.launchpad.net/ubuntu-mozilla-daily/firefox-aurora/ubuntu/
.. _their API: https://stripe.com/docs/api

Introduction
============

 During Stripe_ -processed signup for Our services three  
 Principal Actors send Messages to each other.  This document describes the 
 expected behavior of these "Principals" and Messages in each of several cases. 

 With respect to the behaviors within its scope, it is intended to be
 authoritative (i.e. look here first!), comprehensive (let's not leave any
 relevant part out!), and clear (this doc should reify our common
 understanding). In short, this is the place to explain how it works. 

 A note on the format:
   Each Case contains a sequence of behaviors the reader
   can engage in, in order to (re)produce the case. This reproduction
   will produce output similar to that included in the Case.

Caveat Emptor
-------------

 This is a Message-Centric analysis.  That seemed to be a reasonable
 framework for carving up the system.  I make no claim that it's in-any-sense optimal.

In All Cases
------------

.. _manipulates cookies: http://piwik.org/faq/general/#faq_146
.. _our analytics server: https://analytics.leastauthority.com/piwik/piwik.php?idsite=1

  #. Start Your Local Test Server With the Stripe Signup Implementation:

     #. ``cd YOUR_leastauthority.com_REPO``
     #. ``git checkout 103_implement_stripe_01``
     #. ``git pull git@github.com:LeastAuthority/leastauthority.com.git``
     #. ``cd YOURLOCALPATH/leastauthority.com ./runsite.sh --dev``

  #. Setup up the monitoring environment:

     #. Server Monitoring:

        #. Leave the terminal where ``./runsite.sh --dev`` was executed open
        #. ``tail -F ../sitelogs``
            This displays the HTTP requests the server has received
        
     #. Browser Monitoring:

        #. open your favorite browser (Firefox-Aurora)
        #. open the browser's webconsole (Ctrl-Shift-K)
        #. deselect CSS, JS, Security, and Logging tabs

  #. Navigate to the test site:

     #. paste the following into the browser's location bar
         ``http://localhost:8000``

     Notice attributes of the consequent http traffic:

     - no cookies are set on the browser by the LA server

     .. NOTE:  We need to verify this exhaustively.  To that end I've started
     .. implementing a MITM'd option to runsite.sh --dev that runs the server
     .. "through" a TCP proxy, which dumps all traffic.  Once that's complete
     .. we can grep the logs for patterns like "Set-Cookie"

     - `our analytics server`_, `manipulates cookies`_, it:
        responds to GETs with a Set-Cookie header this
         - sets ``_pk_uid`` to "deleted" with an expiration time in the past 
         - effectively removes ``_pk_uid`` from the browser IIUC
         - looks like this, for a time *after* the '``expires``' value:   
            ``"_pk_uid=deleted; expires=Mon, 12-Nov-2012 19:15:32 GMT"``
         - XXX: Discuss: Is done to enhance User privacy?

     - all requests against resources hosted on different domains are over https

       .. this also needs some more comprehensive proof that manual console inspection 
       .. how is the origin defined?

  #. Navigate to The Start Point:

     #. click the "Sign up!" button, and get 'redirected' to
         ``http://localhost:8000/subscribing``

     Notice attributes of the consequent http traffic:

     - as above
     - the stripe js is fetched over https
     - the jquery source is loaded from a local static file
     - XXX: Discuss:  what's the right policy wrt these 2 options?

 
Case One: Valid CC All Nodes Available
--------------------------------------

.. _'payment-form' HTML form: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/templates/subscription_signup.html#L8

.. _form submission method call: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/content/static/js/subscription_signup.js#L35

Here's a set of valid form input data:

 Email address:
   test@test
 Name:
   testvalidCC
 Card number:
   4242 4242 4242 4242
 CVC:
   111
 Expiration:
   01 / 2015

Stripe API Request:
```````````````````

.. _The Stripe.createToken call: https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/content/static/js/subscription_signup.js#L18
.. _jsonp: http://www.json-p.org/

 #.  Comment out the `form submission method call`_.

 #.  reload the page

 #.  Enter the data into the `'payment-form' HTML form`_ and click "Purchase"

Result 1:
~~~~~~~~~

`The Stripe.createToken call`_ causes the browser to submit an HTTP message with attributes similar to the following:

    METHOD: GET 

    URL:
     ``https://api.stripe.com/v1/tokens?card[number]=4242+4242+4242+4242&card[cvc]=111&card[exp_month]=02&card[exp_year]=2015&key=pk_test_czwzkTp2tactuLOEOqbMTRzG&callback=sjsonp1384288955781&_method=POST``

     The components (excluding delimiters) are as follows:

      #. ``https``
          The protocol, everything besides the hostname is PKI encrypted.
      #. ``api.stripe.com`` See: https://stripe.com/docs/api#intro
          Stripe's api server(s)
      #. ``/v1/tokens``
          the resource url for token handling
      #. ``card[number]=4242+4242+4242+4242&card[cvc]=111&card[exp_month]=02&card[exp_year]=2015``
          the submitted cc information
      #. ``key=pk_test_czwzkTp2tactuLOEOqbMTRzG``
          a unique ID that allows the stripe servers to associated the
          request with our account.
      #. ``callback=sjsonp1384288955781``
          the `jsonp`_ callback registered to handle the jsonp response
           - this function name had an invariant prefix of ``'sjsonp13842'``
             across multiple calls to Stripe.createToken
           - I guess this means that the function name has < 27 bits of entropy 
      #. ``_method=POST``
          XXX: Discuss: I don't know what this signifies.

    HTTP Headers:

     - Request:

      ::

       User-Agent:      Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
       Referer:	        http://localhost:8000/subscribing
       Host:	        api.stripe.com
       DNT:             1
       Connection:      keep-alive
       Accept-Language: en-US,en;q=0.5
       Accept-Encoding: gzip, deflate
       Accept:          */*

     - Response:  

      ::

       Vary:                              Accept-Encoding
       Strict-Transport-Security:         max-age=31556926; includeSubDomains
       Server:                            nginx
       Date:                              Tue, 12 Nov 2013 21:37:35 GMT
       Content-Type:                      application/javascript;charset=utf-8
       Content-Length:                    297
       Content-Encoding:                  gzip
       Connection:                        keep-alive
       Cache-Control:                     no-cache, no-store
       Access-Control-Max-Age:            300
       Access-Control-Allow-Methods:      GET, POST, HEAD, OPTIONS, DELETE
       Access-Control-Allow-Credentials:  true

    Body:

     ::

      sjsonp1384292298925({ "id": "tok_2vgDb9eq6p7Zih", 
                            "livemode": false, 
                            "created": 1384292252, 
                            "used": false, 
                            "object": "token", 
                            "type": "card", 
                            "card": { "id": "card_2vgD4Mq0j1TOvH", 
                                      "object": "card", 
                                      "last4": "4242", 
                                      "type": "Visa", 
                                      "exp_month": 1, 
                                      "exp_year": 2015, 
                                      "fingerprint": "qhjxpr7DiCdFYTlH", 
                                      "customer": null, 
                                      "country": "US", 
                                      "name": null, 
                                      "address_line1": null, 
                                      "address_line2": null, 
                                      "address_city": null, 
                                      "address_state": null, 
                                      "address_zip": null, 
                                      "address_country": null 
                                    } 
                          }, 
                          200
                         ) 

Stripe API Response Handling:
`````````````````````````````

 #.  Uncomment the `form submission method call`_.

 #.  reload the page

 #.  Enter the data into the `'payment-form' HTML form`_ and click "Purchase"


Result 2:
~~~~~~~~~

 The flow now proceeds beyond the response from https://api.stripe.com
 including the information that the card has been verified.

 The next message is passed from the `/subscribing`_ resource to the
 `/subscription-complete`_ resource.

    METHOD: POST

    URL: ``http://localhost:8000/subscription-complete``

    HTTP Headers:

     - Request:

      ::

       User-Agent:      Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
       Referer:         http://localhost:8000/subscribing
       Host:            localhost:8000
       Connection:      keep-alive 
       Accept-Language: en-US,en;q=0.5 
       Accept-Encoding: gzip, deflate
       Accept:          text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8

     - Response:

      ::

       Transfer-Encoding: chunked
       Server:            TwistedWeb/13.1.0
       Date:              Wed, 13 Nov 2013 00:16:29 GMT
       Content-Type:      text/html

    Sent Form Data:

      ::

       stripeToken:     tok_2vilczNdS92TU3
       ProductName:
       pgp_pubkey:
       nickname:        test
       email:           test@test

    Body:

      See https://github.com/LeastAuthority/leastauthority.com/blob/103_implement_stripe_01/lae_site/templates/payment_verified.html
     
Definitions
-----------

  LA Webserver
    `Our interface to the Web`_

  Principals Actors (aka Principals)
    Entities that send and receive Messages during the signup process.

    1. the LA Webserver `Twisted Web Resource`_ s, Provided By 

       - `/subscribing`_
       - `/subscription-complete`_
  
    2. The User's Browser (I usually use `Aurora`_, so it's the most tested)
    3. Stripe Servers via `their API`_
   
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


