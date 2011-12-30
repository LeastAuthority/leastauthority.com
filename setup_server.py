#!/usr/bin/python

import sys

from lae_automation.server import install_server, bounce_server


if len(sys.argv) < 9:
    print "Usage: python setup_server.py PUBLIC_IP KEY_FILE PRIVATE_IP USER_ACCESS_KEY_ID USER_SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME [--no-install]"
    print "Happy set-upping!"
    sys.exit(1)

publicip = sys.argv[1]
ec2keyfilename = sys.argv[2]
privateip = sys.argv[3]
useraccesskeyid = sys.argv[4]
usersecretkey = sys.argv[5]
usertoken = sys.argv[6]
producttoken = sys.argv[7]
bucketname = sys.argv[8]

if "--no-install" not in sys.argv:
    install_server(publicip, ec2keyfilename, sys.stdout, sys.stderr)

bounce_server(publicip, ec2keyfilename, privateip, useraccesskeyid, usersecretkey, usertoken, producttoken, bucketname,
              sys.stdout, sys.stderr, sys.stderr)
