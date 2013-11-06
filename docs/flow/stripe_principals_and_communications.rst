.. _Stripe: https://stripe.com
.. _Stripe JS: https://stripe.com/docs/stripe.js
.. _Stripe Python Library: https://github.com/stripe/stripe-python
.. _root resource: https://twistedmatrix.com/documents/current/web/howto/web-in-60/static-dispatch.html

Introduction
============

 During signup for Least Authority services, in which Stripe_ is used as the
 payment processor, there are several network nodes that communicate with
 each other.  The communication is conducted via HTTP messages.  This
 document describes the expected nodes and messages in each of several cases.

Definitions
-----------

- Service: The product we are providing for a fee.

- User:  The party signing up for the service

- Stripe Servers: The agency managing credit card payments processing,
  verification, and billing. 

- `Stripe Python Library`_: The Python Library Installed on the Least
  Authority Server (via pip)

- `Stripe JS`_: The JS loaded into the Users browser

Overview
--------

 Each case begins with a browser which has loaded the content provided in
 response to an HTTP GET request against the "`root resource`_" at:

  ``https://leastauthority.com``

Case One: Valid CC All Nodes Available
``````````````````````````````````````



