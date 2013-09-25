#!/usr/bin/env python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_automation.initialize import create_stripe_user_bucket


if len(sys.argv) < 4:
    print "Usage: python create_stripe_user_bucket.py USER_ACCESS_KEY_ID USER_SECRET_KEY BUCKET_NAME [LOCATION]"
    print "Happy bucket-creating!"
    sys.exit(1)

useraccesskeyid = sys.argv[1]
usersecretkey = sys.argv[2]
bucketname = sys.argv[3]
if len(sys.argv) > 5:
    location = sys.argv[5]
else:
    location = None  # default

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = create_stripe_user_bucket(useraccesskeyid, usersecretkey, bucketname, sys.stdout, sys.stderr,
                              location=location)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
