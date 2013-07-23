#!/usr/bin/env python

import os, sys, simplejson

from twisted.internet import reactor
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath

from lae_automation.monitor import check_user_bucket


if len(sys.argv) < 1:
    print "Usage: python check_user_bucket.py USER_SECRET_FILENAME"
    #USER_ACCESS_KEY_ID USER_SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME"
    print "Happy bucket-checking!"
    sys.exit(1)

print

raw_secrets = simplejson.load(FilePath(sys.argv[1]).open())

secrets = {}
for key in ['access_key_id', 
            'secret_key', 
            'user_token', 
            'bucket_name', 
            'product_token']:
    secrets[key] = raw_secrets[key]
secrets['stdout'] = sys.stdout
secrets['stderr'] = sys.stderr

def cb(x):
    print 'Callbacks fired, and check_user_bucket returned:'
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print "Failure with a x.value = 'response'!"
        print x.value.response

d = check_user_bucket(**secrets)

#useraccesskeyid, usersecretkey, usertoken, bucketname, sys.stdout, sys.stderr,
#                      producttoken=producttoken)

d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
