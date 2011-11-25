#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials

from lae_automation.initialize import deploy_EC2_instance


if len(sys.argv) < 7:
    print "Usage: python deploy.py ACCESS_KEY_ID SECRET_KEY AMI_IMAGE_ID INSTANCE_SIZE BUCKET_NAME KEYPAIR_NAME"
    print "Happy deploying!"
    sys.exit(1)

accesskeyid = sys.argv[1]
secretkey = sys.argv[2]
amiimageid = sys.argv[3]
instancesize = sys.argv[4]
bucketname = sys.argv[5]
keypairname = sys.argv[6]
creds = AWSCredentials(accesskeyid, secretkey)

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = deploy_EC2_instance(creds, EC2_ENDPOINT, amiimageid, instancesize, bucketname, keypairname, cb)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
