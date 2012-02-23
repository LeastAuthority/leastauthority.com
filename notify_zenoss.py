#!/usr/bin/python

import sys
from lae_automation.server import notify_zenoss

if len(sys.argv) < 4:
    print "Usage: python notify_zenoss.py EC2PUBIP ZENOSSSERVERIP ZENOSSKEYSPATH"
    print "Happy notifying the Zenoss server!"
    sys.exit(1)

ec2pubip = sys.argv[1]
zenoss_IP = sys.argv[2]
zenoss_privkey_path= sys.argv[3]

notify_zenoss(ec2pubip, zenoss_IP, zenoss_privkey_path)
