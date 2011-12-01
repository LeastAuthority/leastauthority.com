#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials

from lae_automation.initialize import delete_user_bucket


if len(sys.argv) < 6:
    print "Usage: python delete_bucket.py ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME"
    print "Bye bye bucket!"
    sys.exit(1)

accesskeyid = sys.argv[1]
secretkey = sys.argv[2]
usertoken = sys.argv[3]
producttoken = sys.argv[4]
bucketname = sys.argv[5]

usercreds = AWSCredentials(accesskeyid, secretkey)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = delete_user_bucket(usercreds, usertoken, bucketname, sys.stdout, sys.stderr,
                       producttoken=producttoken)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
