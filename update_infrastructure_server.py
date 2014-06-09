#!/usr/bin/env python
"""This script:
(1) updates an infrastructure EC2 instance
"""

import os, sys, argparse

from twisted.internet import defer
from twisted.python.filepath import FilePath

from lae_automation.signup import EC2_ENDPOINT
from lae_automation.server import install_infrastructure_server


parser = argparse.ArgumentParser(description="Update an infrastructure server.")

parser.add_argument("host_IP_address", help="The IP (v4) address of the web server. ")

parser.add_argument("admin_keypair_name", help=
                    "The name of the keypair (as configured in the AWS account) for the new server. "
                    "Unused if --existing_host is specified.")
parser.add_argument("admin_privkey_path", help=
                    "The path to the private key (.pem) file of the keypair for the new server. "
                    "Unused if --existing_host is specified.")

parser.add_argument("leastauthority_repo_path", help=
                    "The absolute path to the git repository containing the leastauthority.com "
                    "code to deploy.")

parser.add_argument("leastauthority_commit_ref", help=
                    "The specific commit within the repository at 'leastauthority_repo_path' that "
                    "will be deployed.")

parser.add_argument("secret_config_repo_path", help=
                    "The absolute path to the git repository containing the secret configuration to "
                    "deploy.")
parser.add_argument("secret_config_commit_ref", help=
                    "The specific commit within the repository at 'secret_config_repo_path' that will "
                    "be deployed.")

args = parser.parse_args()
print "args: %r" % (args,)


admin_keypair_name = "ec2sshadmin"
admin_privkey_path = "../secret_config/ec2sshadmin.pem"
leastauthority_repo_path = args.leastauthority_repo_path
leastauthority_commit_ref = args.leastauthority_commit_ref
secret_config_repo_path = args.secret_config_repo_path
secret_config_commit_ref = args.secret_config_commit_ref
existing_host = args.existing_host
new_host = args.new_host

endpoint_uri = EC2_ENDPOINT
bucket_name = 'dummy'
website_pubkey = None
stdout = sys.stdout
stderr = sys.stderr

ec2_accesskeyid = FilePath(ec2_accesskeyid_path).getContent().strip()
ec2_secret      = FilePath(ec2_secret_path).getContent().strip()

def printer(x):
    """This handy function let's us see what's going on between calls in the callback chain."""
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    """This handy function let's us see what's going on between errors in the callback chain."""
    print >> sys.stderr, "Error returned ?"
    print >> sys.stderr, x


d = defer.succeed(install_infrastructure_server(
        existing_host, admin_privkey_path, website_pubkey,
        leastauthority_repo_path, leastauthority_commit_ref,
        secret_config_repo_path, secret_config_commit_ref,
        stdout, stderr))


d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run()
