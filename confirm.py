#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_automation.confirmation import send_signup_confirmation


if len(sys.argv) < 4:
    print "Usage: python confirm.py CUSTOMER_NAME CUSTOMER_EMAIL INTRODUCER_FURL [CUSTOMER_KEYINFO]"
    print "Happy confirmation-sending!"
    sys.exit(1)

customer_name = sys.argv[1]
customer_email = sys.argv[2]
external_introducer_furl = sys.argv[3]
if len(sys.argv) > 4:
    customer_keyinfo = sys.argv[4]
else:
    customer_keyinfo = ''

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = send_signup_confirmation(customer_name, customer_email, external_introducer_furl, customer_keyinfo,
                             sys.stdout, sys.stderr)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
