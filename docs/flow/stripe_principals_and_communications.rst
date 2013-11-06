.. _Stripe: https://stripe.com
.. _Stripe JS: https://stripe.com/docs/stripe.js

Introduction
============

 During signup for Least Authority services in which Stripe_ is used as the
 payment processor, there are several network nodes, that communicate with
 each other.  The communication is conducted via HTTP messages.  This
 document describes the expected nodes and messages in each of several cases.

Definitions
-----------

- Service: The product we are providing for a fee.

- User:  The party signing up for the service

- Stripe Servers: The agency managing credit card payments processing,
  verification, and billing. 

- Stripe API:  The Python Library Installed on the Least Authority Server

- `Stripe JS`_: The JS loaded into the Users browser
