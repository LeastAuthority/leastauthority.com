#!/usr/bin/python

import sys, os

from twisted.internet import reactor
from twisted.python.filepath import FilePath

from lae_automation.config import Config
from lae_automation.monitor import check_servers, read_serverinfo, compare_servers_to_local, \
    monitoring_check
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser


endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
config = Config()

ec2secretpath='../secret_config/ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()
serverinfocsvpath = '../serverinfo.csv'
lasterrorspath = '../lasterrors.txt'

monitor_privkey_path = str(config.other['monitor_privkey_path'])

serverinfotuple = read_serverinfo(serverinfocsvpath)
localstate = {}
for propertytuple in serverinfotuple:
    (launch_time, instance_id, publichost, status) = propertytuple
    localstate[instance_id] = (launch_time, publichost, status)

POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

def checker(stdout, stderr):
    d = wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri,
                                ServerInfoParser(('launchTime', 'instanceId'), ('dnsName', 'instanceState.name')),
                                POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, stderr)

    d.addCallback(lambda remoteproperties: compare_servers_to_local(remoteproperties, localstate, stdout, stderr))

    d.addCallback(lambda host_list: check_servers(host_list, monitor_privkey_path, stdout, stderr))

    return d

d = monitoring_check(checker, lasterrorspath, "storage servers", sys.stdout, sys.stderr)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
