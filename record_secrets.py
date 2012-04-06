#!/usr/bin/python

import sys

from lae_automation.server import record_secrets


if len(sys.argv) < 3:
    print "Usage: python record_secrets.py PUBLIC_IP TIMESTAMP"
    print "Happy recording!"
    sys.exit(1)

publicip = sys.argv[1]
timestamp = sys.argv[2]
ec2keyfilename = "../EC2adminkeys2.pem"

record_secrets(publicip, timestamp, ec2keyfilename, sys.stdout, sys.stderr)
