#!/usr/bin/env python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath

from lae_automation.config import Config
from lae_automation.initialize import create_stripe_user_bucket

if len(sys.argv) != 2:
    print "Usage: python create_stripe_user_bucket.py BUCKETNAME"
    print "Happy bucket-creating!"
    sys.exit(1)

conf = Config()
useraccesskeyid = conf.other['ec2_access_key_id']
usersecretkey = FilePath(conf.other['ec2_secret_path']).getContent().rstrip()
bucketname = sys.argv[1]

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = create_stripe_user_bucket(useraccesskeyid, usersecretkey, bucketname, sys.stdout, sys.stderr,
                              location='http://s3.amazonaws.com')
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
