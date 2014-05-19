#!/usr/bin/env python
"""This script:
(1) launches a new EC2 instance
(2) provisions it with a version of the leastauthority.com repository
(3) provisions it with a version of the secret_config repository
(4) launches the webserver specified in those repositories

  AWS credentials, ami parameters (e.g. size and ami id) and metadata to associate with the instance
(e.g. instance name) are parsed from the lae_automation_config.json configuration file.
"""

import os, sys, argparse
from twisted.python.filepath import FilePath
from lae_automation.initialize import deploy_infrastructure_EC2
from lae_automation.signup import EC2_ENDPOINT
from lae_automation.server import install_infrastructure_server
from twisted.internet import defer


# https://cloud-images.ubuntu.com/locator/ec2/
# We need an amd64 EBS instance. Trusty is an LTS release.
# http://stackoverflow.com/questions/22130214/amazon-ec2-ubuntupv-or-ubuntuhvm recommends PV over HVM.
DEFAULT_AMI_IMAGE_ID = 'ami-018c9568'

# http://aws.amazon.com/ec2/instance-types/
DEFAULT_INSTANCE_SIZE = 'm3.medium'

parser = argparse.ArgumentParser(description=
                    "Deploy a new infrastructure server. Each repository-and-reference "
                    "(e.g. leastauthority.com-and-SHA1) is specified as a path to the "
                    "repository followed by a reference to the specific commit to be "
                    "deployed.")

parser.add_argument("ec2_secret_path", help=
                    "The absolute path to the EC2 provisioning secret that authorizes deployment "
                    "of the infrastructure server.")
parser.add_argument("ec2_accesskeyid_path", help=
                    "The absolute path to the EC2 access key id file.")

parser.add_argument("--ami_image_id", default=DEFAULT_AMI_IMAGE_ID, help=
                    "The AMI image ID to use for the new server (default: %r)." % (DEFAULT_AMI_IMAGE_ID,))
parser.add_argument("--instance_size", default=DEFAULT_INSTANCE_SIZE, help=
                    "The instance size to use for a new server (default: %r)." % (DEFAULT_INSTANCE_SIZE,))

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

exc_group = parser.add_mutually_exclusive_group()
exc_group.add_argument('--existing_host', type=str)
exc_group.add_argument('--new_host', type=str, help=
                       "How this instance is referred to e.g. via AWS console.")

args = parser.parse_args()
print "args: %r" % (args,)


ec2_secret_path = args.ec2_secret_path
ec2_accesskeyid_path = args.ec2_accesskeyid_path
ami_image_id = args.ami_image_id
instance_size = args.instance_size
admin_keypair_name = args.admin_keypair_name
admin_privkey_path = args.admin_privkey_path
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

if existing_host:
    d = defer.succeed(install_infrastructure_server(
                                  existing_host, admin_privkey_path, website_pubkey,
                                  leastauthority_repo_path, leastauthority_commit_ref,
                                  secret_config_repo_path, secret_config_commit_ref,
                                  stdout, stderr))
elif new_host:
    d = deploy_infrastructure_EC2(ec2_accesskeyid, ec2_secret, endpoint_uri,
                                  ami_image_id, instance_size, bucket_name,
                                  admin_keypair_name, new_host,
                                  admin_privkey_path, website_pubkey,
                                  leastauthority_repo_path, leastauthority_commit_ref,
                                  secret_config_repo_path, secret_config_commit_ref,
                                  stdout, stderr, clock=None)


d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run()
