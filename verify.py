#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_automation.initialize import verify_user_account


if len(sys.argv) < 5:
    print "Usage: python verify.py USER_ACCESS_KEY_ID USER_SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN"
    print "Trust, but verify!"
    sys.exit(1)

useraccesskeyid = sys.argv[1]
usersecretkey = sys.argv[2]
usertoken = sys.argv[3]
producttoken = sys.argv[4]

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = verify_user_account(useraccesskeyid, usersecretkey, usertoken, producttoken, sys.stdout, sys.stderr)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
