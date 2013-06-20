#! /usr/bin/env python

import sys, os, traceback, subprocess
from twisted.python.filepath import FilePath
from twisted.python.failure import Failure
from twisted.internet import reactor

from lae_automation.config import Config
from lae_automation.aws.queryapi import wait_for_EC2_properties, ServerInfoParser, pubIPextractor


endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
config = Config()

ec2secretpath='../secret_config/ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

monitor_privkey_path = str(config.other['monitor_privkey_path'])
admin_privkey_path = str(config.other['admin_privkey_path'])


POLL_TIME = 10
ADDRESS_WAIT_TIME = 60

d = wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri,
                            ServerInfoParser(('launchTime', 'instanceId'), ('dnsName',)),
                            POLL_TIME, ADDRESS_WAIT_TIME, sys.stdout, sys.stderr)

def update_known_hosts(publichost):
    arg_list = ['ssh', '-o', 'StrictHostKeyChecking=no', '-i', monitor_privkey_path, 'monitor@%s' % (publichost,), 'exit']
    process = subprocess.Popen(arg_list)
    retval = process.wait()
    if retval != 0:
        print >>sys.stderr, ("Warning: Attempt to ssh to %s returned exit code %s." % (publichost, retval))

def upgrade_servers(remotepropstuplelist):
    for rpt in remotepropstuplelist:
        publichost = pubIPextractor(rpt[2])
        if not publichost:
            print >>sys.stderr, ("Warning: Host launched at %s with instance ID %s has no public IP (maybe it has been terminated)."
                                 % (rpt[0], rpt[1]))
        else:
            print "Checking and maybe updating %r..." % (publichost,)
            try:
                update_known_hosts(publichost)
            except:
                traceback.print_exc()

d.addCallback(upgrade_servers)


def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()

