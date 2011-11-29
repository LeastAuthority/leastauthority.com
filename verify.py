#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials

from lae_automation.initialize import verify_user_account


if len(sys.argv) < 5:
    print "Usage: python verify.py ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN"
    print "Trust, but verify!"
    sys.exit(1)

access_key_id = sys.argv[1]
secret_key = sys.argv[2]
user_token = sys.argv[3]
product_token = sys.argv[4]
creds = AWSCredentials(access_key_id, secret_key)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = verify_user_account(creds, user_token, product_token, sys.stdout, sys.stderr)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
