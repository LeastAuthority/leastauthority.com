#!/usr/bin/python

import sys, os

from twisted.python.filepath import FilePath
from twisted.python.failure import Failure
from twisted.internet import reactor

from lae_automation.config import Config
from lae_automation.monitor import write_serverinfo
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser


endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
configpath='../lae_automation_config.json'
config = Config(configpath)

ec2secretpath='../ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

monitor_privkey_path = str(config.other['monitor_privkey_path'])

POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

d = wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri,
                            ServerInfoParser(('launchTime', 'instanceId'), ('dnsName',)),
                            POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, sys.stderr)

# User should manually rename to serverinfo.csv if correct.
d.addCallback(lambda remoteproperties: write_serverinfo("new_serverinfo.csv", remoteproperties))

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
