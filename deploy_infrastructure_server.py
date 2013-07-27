#!/usr/bin/env python
"""This script:
(1) launches a new EC2 instance
(2) provisions it with a version of the leastauthority.com repository
(3) provisions it with a version of the secret_config repository
(4) launches the webserver specified in those repositories

Some AWS credentials are parsed from the lae_automation_config.json
configuration file.
"""

import sys, argparse
#import os

from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.initialize import deploy_infrastructure_EC2 
from lae_automation.signup import EC2_ENDPOINT
from lae_automation.server import install_infrastructure_server
from twisted.internet import defer

parser = argparse.ArgumentParser(description="\
Deploy a new infrastructure server. You must specify each necessary \
repository-and-reference (e.g. leastauthority.com-and-SHA1) as a space-delimited \
pair of path_to_repository, and reference to the specific commit hash you want \
deployed.")

parser.add_argument("ec2secret_paths", help="\
This space-delimited pair consists of two parts:  First: path to the EC2 provisioning \
secret that authorizes deployment of the infrastructure server.  Second: the path \
to the access key id file.", nargs=2)

parser.add_argument("ami_image_id", help="\
The AMI image ID to use for the new server.")

parser.add_argument("instance_size", help="\
The instance size to use for the new server (e.g. 't1.micro' or 'm1.small').")

parser.add_argument("leastauthority_com_version_ID", help="\
This space-delimited pair specifies a commit, and consists of:  First: the absolute \
path to the git repository that contains the leastauthority.com code to deploy. \
Second: the reference to the specific commit hash, within that repository, that \
will be deployed.", nargs=2)

parser.add_argument("secrets_version_ID", help="\
This space-delimited pair specifies a commit, and consists of:  First: the absolute \
path to the git repository that contains the secret_config code to deploy. \
Second: the reference to the specific commit, within that repository, that will \
be deployed.", nargs=2)

exc_group = parser.add_mutually_exclusive_group()
exc_group.add_argument('--existing_host', type=str)
exc_group.add_argument('--new_host', type=str, help="\
How this instance will be referred to, e.g. in the AWS console.")

args = parser.parse_args()
print "args: %s" % (args,)
print

ec2secretpath = args.ec2secret_paths[0]
ec2accesskeyidpath = args.ec2secret_paths[1]
ami_image_id = args.ami_image_id
instance_size = args.instance_size
leastauthority_repo_path = args.leastauthority_com_version_ID[0]
leastauth_commit_ref = args.leastauthority_com_version_ID[1]
secret_conf_repo_path = args.secrets_version_ID[0]
secrets_commit_ref = args.secrets_version_ID[1]
existing_host = args.existing_host
new_host = args.new_host

# XXX configpath is a function of the secret_config repo specified in the arguments!

configpath='../secret_config/lae_automation_config.json'

config = Config(configpath)

keypair_name = str(config.other['admin_keypair_name'])
admin_privkey_path = str(config.other['admin_privkey_path'])
endpoint_uri = EC2_ENDPOINT
bucket_name = 'dummy'
website_pubkey = None
stdout = sys.stdout
stderr = sys.stderr

ec2accesskeyid = FilePath(ec2accesskeyidpath).getContent().strip()
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

def printer(x):
    """This handy function let's us see what's going on between calls in the callback chain."""
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    """This handy function let's us see what's going on between errors in the callback chain."""
    print >>sys.stderr, "Error returned"
    print >>sys.stderr, x

if existing_host:
    d = defer.succeed(install_infrastructure_server(
                        existing_host, admin_privkey_path, website_pubkey,
                        leastauthority_repo_path, leastauth_commit_ref,
                        secret_conf_repo_path, secrets_commit_ref, stdout,
                        stderr ) )
elif new_host:
    d = deploy_infrastructure_EC2(ec2accesskeyid, ec2secretkey, endpoint_uri,
                                  ami_image_id, instance_size, bucket_name,
                                  keypair_name, new_host,
                                  admin_privkey_path, website_pubkey,
                                  leastauthority_repo_path,
                                  leastauth_commit_ref,
                                  secret_conf_repo_path, secrets_commit_ref,
                                  stdout, stderr, clock=None)


d.addCallbacks(printer, eb)
#d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run() 

