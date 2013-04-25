#!/usr/bin/env python

import sys, os
from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.aws.queryapi import wait_for_EC2_sshfp


if len(sys.argv) < 2:
    print "Usage: python get_fp_from_console.py INSTANCE_ID"
    print "NOTE: Console output is nominally available for 1 hour following a boot, reboot, or termination."
    print "Happy fingerprint getting! :-)"
    sys.exit(1)


instance_id = sys.argv[1]

config = Config()
ec2secretpath = '../secret_config/ec2secret'

# There are currently two different ec2 access secrets (keys) one for testing and one for production.
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

# AMI's are "endpoint aware", i.e. specific to a region.
EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'

# Interval between requests in seconds
POLLING_INTERVAL = 5

# Total amount time spend querying in seconds.
WAIT_TIME = 15

def eb(f):
    print >>sys.stderr, "Error returned from wait_for_EC2_sshfp invocation!"
    print >>sys.stderr, f

def printer(x):
    print "x is %s" % (x,)
    return x

d = wait_for_EC2_sshfp(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, POLLING_INTERVAL, WAIT_TIME, 
                       sys.stdout, sys.stderr, instance_id)

d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor
reactor.run()
