#!/usr/bin/python

import sys

from twisted.python.filepath import FilePath

from lae_automation.server import install_server, bounce_server


if len(sys.argv) < 9:
    print "Usage: python setup_server.py PUBLIC_IP PRIVATE_IP USER_ACCESS_KEY_ID USER_SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME SECRETS_PATH [--no-install]"
    print "Happy set-upping!"
    sys.exit(1)

publicip = sys.argv[1]
privateip = sys.argv[2]
useraccesskeyid = sys.argv[3]
usersecretkey = sys.argv[4]
usertoken = sys.argv[5]
producttoken = sys.argv[6]
bucketname = sys.argv[7]
secretspath = sys.argv[8]

ec2_privkey_path = '../EC2adminkeys2.pem'
monitor_pubkey = FilePath('../EC2monitorkeys2.pub').getContent().strip()
monitor_privkey_path = '../EC2monitorkeys2.pem'

try:
    secretsfile = open(secretspath, 'a')

    if "--no-install" not in sys.argv:
        install_server(publicip, ec2_privkey_path, monitor_pubkey, monitor_privkey_path, sys.stdout, sys.stderr)

    bounce_server(publicip, ec2_privkey_path, privateip, useraccesskeyid, usersecretkey, usertoken, producttoken, bucketname,
                  None, sys.stdout, sys.stderr, secretsfile)
finally:
    secretsfile.close()
