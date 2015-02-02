#!/usr/bin/python

import sys, os

from twisted.internet import reactor
from twisted.python.filepath import FilePath

from lae_automation.config import Config
from lae_automation.monitor import check_servers, read_serverinfo, compare_servers_to_local, \
    monitoring_check
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser


if len(sys.argv) < 2:
    print "Usage: python multiservercheck.py SERVICE"
    print "Happy checking!"
    sys.exit(1)

service = sys.argv[1]

endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
config = Config()

service_lower = service.lower()
ssec2_secret_path = '../secret_config/%s_secret' % (service_lower,)
ssec2_accesskeyid_path = '../secret_config/%s_accesskeyid' % (service_lower,)

ssec2_secret = FilePath(ssec2_secret_path).getContent().strip()
ssec2_accesskeyid = FilePath(ssec2_accesskeyid_path).getContent().strip()
serverinfo_csv_path = '../%s_serverinfo.csv' % (service_lower,)
lasterrors_path = '../%s_lasterrors.txt' % (service_lower,)

monitor_privkey_path = '../secret_config/%s_monitorkeys.pem' % (service_lower,)

serverinfo_tuple = read_serverinfo(serverinfo_csv_path)
local_state = {}
for property_tuple in serverinfo_tuple:
    (launch_time, instance_id, publichost, status) = property_tuple
    local_state[instance_id] = (launch_time, publichost, status)

POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

def _print(x):
    print x
    return x

def checker(stdout, stderr):
    d = wait_for_EC2_properties(ssec2_accesskeyid, ssec2_secret, endpoint_uri,
                                ServerInfoParser(('launchTime', 'instanceId'), ('dnsName', 'instanceState.name')),
                                POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, stderr)
    d.addCallback(_print)

    d.addCallback(lambda remote_properties: compare_servers_to_local(remote_properties, local_state, stdout, stderr))

    d.addCallback(lambda host_list: check_servers(host_list, monitor_privkey_path, stdout, stderr))

    return d

d = monitoring_check(checker=checker, lasterrors_path=lasterrors_path,
                     from_email="info@leastauthority.com",
                     what="storage servers",
                     stdout=sys.stdout, stderr=sys.stderr)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
