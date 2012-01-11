#!/usr/bin/python

import sys, os
from cStringIO import StringIO

from twisted.python.filepath import FilePath
from twisted.python.failure import Failure
from twisted.internet import reactor

from lae_automation.config import Config
from lae_automation.monitor import check_servers, readserverinfocsv, comparetolocal
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser


endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
configpath='../lae_automation_config.json'
config = Config(configpath)

ec2secretpath='../ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()
serverinfocsvpath = 'serverinfo.csv'

monitor_privkey_path = str(config.other['monitor_privkey_path'])

stderr = StringIO()

serverinfotuple = readserverinfocsv(serverinfocsvpath)
localstate = {}
for propertytuple in serverinfotuple:
    localstate[propertytuple[2]] = (propertytuple[0], propertytuple[1])

propertiesofinterest = ('launchTime', 'instanceId', 'dnsName', 'privateDnsName')
POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

d = wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri, ServerInfoParser(propertiesofinterest),
                            POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, sys.stderr)

d.addCallback(lambda remoteproperties: comparetolocal(remoteproperties, localstate, sys.stdout, sys.stderr))

d.addCallback(lambda host_list: check_servers(host_list, monitor_privkey_path, sys.stdout, stderr))
def _print_errors(success):
    print >>sys.stderr, stderr.getvalue()
    if not success:
        raise Exception("There was at least one failure.")
d.addCallback(_print_errors)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
