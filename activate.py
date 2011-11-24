#!/usr/bin/python

import sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_automation.initialize import activate_user_account_desktop


if len(sys.argv) < 3:
    print "Usage: python activate.py ACTIVATION_KEY LONG_PRODUCT_TOKEN"
    print "Happy activating!"
    sys.exit(1)

# TODO: the product token should probably be in a file.
activation_key = sys.argv[1]
product_token = sys.argv[2]

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = activate_user_account_desktop(activation_key, product_token, cb)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
