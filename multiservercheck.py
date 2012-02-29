#!/usr/bin/python

import sys, os
from cStringIO import StringIO

from twisted.python.filepath import FilePath
from twisted.python.failure import Failure
from twisted.internet import reactor

from lae_automation.config import Config
from lae_automation.monitor import check_servers, read_serverinfo, compare_servers_to_local, \
    send_monitoring_report
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser


endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
configpath='../lae_automation_config.json'
config = Config(configpath)

ec2secretpath='../ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()
serverinfocsvpath = 'serverinfo.csv'
lasterrorspath = 'lasterrors.txt'

monitor_privkey_path = str(config.other['monitor_privkey_path'])

stderr = StringIO()

serverinfotuple = read_serverinfo(serverinfocsvpath)
localstate = {}
for propertytuple in serverinfotuple:
    localstate[propertytuple[2]] = (propertytuple[0], propertytuple[1])

lasterrors = None
lasterrorsfp = FilePath(lasterrorspath)
if lasterrorsfp.exists():
    lasterrors = lasterrorsfp.getContent()

POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

d = wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri,
                            ServerInfoParser(('launchTime', 'instanceId'), ('dnsName',)),
                            POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, stderr)

d.addCallback(lambda remoteproperties: compare_servers_to_local(remoteproperties, localstate, sys.stdout, stderr))

d.addCallback(lambda host_list: check_servers(host_list, monitor_privkey_path, sys.stdout, stderr))

def cb(x):
    if isinstance(x, Failure):
        print >>stderr, str(x)
        if hasattr(x.value, 'response'):
            print >>stderr, x.value.response

    errors = stderr.getvalue()
    print >>sys.stderr, errors
    if errors != lasterrors:
        d2 = send_monitoring_report(errors)
        def _sent(ign):
            lasterrorsfp.setContent(errors)
            raise Exception("Sent failure report.")
        def _err(f):
            print >>stderr, str(f)
            return f
        d2.addCallbacks(_sent, _err)
        return d2

d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
