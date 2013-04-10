#!/usr/bin/env python

import sys, os
from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.aws.queryapi import wait_for_EC2_consoleoutput


if len(sys.argv) < 2:
    print "Usage: python console_output_describer.py INSTANCE_ID"
    print "Happy comparing!"
    sys.exit(1)

#Console output is nominally available for 1 hour following a boot, reboot, or termination.
instance_id = sys.argv[1]

config = Config()
ec2secretpath = '../secret_config/ec2secret'

# Configuration which is necessarily shared with other Least Authority EC2s (like SSEC2s)
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

# Configuration which may be specific to the infrastructure server
EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'

# delay between starting an instance and setting its tags
POLLING_INTERVAL = 5
WAIT_TIME = 15

def eb(f):
    print >>sys.stderr, "Error returned from wait_for_EC2_consoleoutput invocation!"
    print >>sys.stderr, f

def printer(x):
    print "x is %s" % (x,)
    return x

d = wait_for_EC2_consoleoutput(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT,
                                                     POLLING_INTERVAL, WAIT_TIME, sys.stdout, sys.stderr,
                                                     instance_id)

d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor
reactor.run()
