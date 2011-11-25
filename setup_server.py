#!/usr/bin/python

import sys

from txaws.service import AWSCredentials

from lae_automation.server import install_server, bounce_server


if len(sys.argv) < 10:
    print "Usage: python setup_server.py PUBLIC_IP KEY_FILE NICKNAME PRIVATE_IP ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME [--no-install]"
    print "Happy set-upping!"
    sys.exit(1)

publicip = sys.argv[1]
ec2keyfilename = sys.argv[2]
privateip = sys.argv[3]
accesskeyid = sys.argv[4]
secretkey = sys.argv[5]
usertoken = sys.argv[6]
producttoken = sys.argv[7]
bucketname = sys.argv[8]

usercreds = AWSCredentials(accesskeyid, secretkey)

if "--no-install" not in sys.argv:
    install_server(publicip, ec2keyfilename, sys.stdout, sys.stderr)

bounce_server(publicip, ec2keyfilename, privateip, usercreds, usertoken, producttoken, bucketname,
              sys.stdout, sys.stderr)
