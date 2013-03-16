#!/usr/bin/env python

import sys, os
from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.aws.queryapi import AddressParser
from lae_automation.initialize import verify_and_store_serverssh_pubkey


if len(sys.argv) < 2:
    print "Usage: python compare_ssh_pubkeys.py INSTANCE_ID"
    print "Happy comparing!"
    sys.exit(1)

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
    print >> sys.stderr, "Error returned from wait_for_EC2_consoleoutput invocation!"
    print >> sys.stderr, f

d = verify_and_store_serverssh_pubkey(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT,
                                      AddressParser(), POLLING_INTERVAL, WAIT_TIME,
                                      sys.stdout, sys.stderr, instance_id)

d.addErrback(eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor
reactor.run()
