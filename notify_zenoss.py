#!/usr/bin/python

import sys
from lae_automation.server import notify_zenoss

if len(sys.argv) < 5:
    print "Usage: python notify_monitor.py EC2INSTANCEID EC2PUBIP ZENOSSSERVERIP ZENOSSKEYSPATH"
    print "Happy notifying the Zenoss server!"
    sys.exit(1)

ec2instanceid = sys.argv[1]
ec2pubip = sys.argv[2]
zenoss_IP = sys.argv[3]
zenoss_privkey_path= sys.argv[4]

notify_zenoss(ec2instanceid, ec2pubip, zenoss_IP, zenoss_privkey_path)
