#!/usr/bin/python

import sys

from twisted.python.filepath import FilePath

from lae_automation.server import record_secrets


if len(sys.argv) < 3:
    print "Usage: python record_secrets.py PUBLIC_HOST TIMESTAMP"
    print "Happy recording!"
    sys.exit(1)

publichost = sys.argv[1]
timestamp = sys.argv[2]
ec2keyfilename = "../secret_config/EC2adminkeys2.pem"

record_secrets(FilePath('..'), publichost, timestamp, ec2keyfilename, sys.stdout, sys.stderr)
