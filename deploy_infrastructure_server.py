#!/usr/bin/env python
"""This script:
(1) launches a new EC2 instance
(2) provisions it with a version of the leastauthority.com repository
(3) provisions it with a version of the secret_config repository
(4) launches the webserver specified in those repositories

  AWS credentials, ami parameters (e.g. size and ami id) and metadata to associate with the instance 
(e.g. instance name) are parsed from the lae_automation_config.json configuration file.
"""

import sys, os, argparse 
from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.initialize import deploy_infrastructure_EC2 
from lae_automation.signup import EC2_ENDPOINT

parser = argparse.ArgumentParser(description="Deploy a new infrastructure server.\nYou must specify each necessary repository-and-reference (e.g. leastauthority.com-and-SHA1) as an ordered pair of path_to_repository, and reference to the specific commit you want deployed.")

parser.add_argument("leastauthority_com_version_ID", help="This ordered parameter-pair consists of two parts, which are sufficient to specify a commit.\nFirst: the absolute path to the git repository which contains the leastauthority.com code to deploy.\nSecond: the reference to the specific commit, within that repository, which will be deployed.", nargs=2)

parser.add_argument("secrets_version_ID", help="This ordered parameter-pair consists of two parts, which are sufficient to specify a commit.\nFirst: the absolute path to the git repository which contains the secret_config code to deploy.\nSecond: the reference to the specific commit, within that repository, which will be deployed.", nargs=2)

args = parser.parse_args()
print "args: %s" % args
print "args.leastauthority_com_version_ID[0]: %s" % args.leastauthority_com_version_ID[0]

leastauthority_repo_path = args.leastauthority_com_version_ID[0]
leastauth_commit_ref = args.leastauthority_com_version_ID[1]
secret_conf_repo_path = args.secrets_version_ID[0]
secrets_commit_ref = args.secrets_version_ID[1]

# XXX configpath is a function of the secret_config repo specified in the arguments!

configpath='../secret_config/lae_automation_config.json'

config = Config(configpath)

#Configuration copied from most recent product
#https://en.wikipedia.org/wiki/Amazon_Machine_Image
ami_image_id = str(config.products[-1]['ami_image_id']) 

instance_size = str(config.products[-1]['instance_size'])
ec2accesskeyid = str(config.other['ec2_access_key_id'])
keypair_name = str(config.other['admin_keypair_name'])
admin_privkey_path = str(config.other['admin_privkey_path'])
ec2secretpath = str(config.other['ec2_secret_path'])
endpoint_uri = EC2_ENDPOINT
bucket_name = 'dummy'
website_pubkey = None
stdout = sys.stdout
stderr = sys.stderr

print config.other['deployment']
#Configuration which is specific to the test account

COMMIT_TAG = '2013-06-20'

instance_name = str(config.other['deployment'][COMMIT_TAG]['instance_name'])

ec2secretkey = FilePath(ec2secretpath).getContent().strip()

def printer(x):
    """This handy function let's us see what's going on between calls in the callback chain."""
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    """This handy function let's us see what's going on between errors in the callback chain."""
    print >> sys.stderr, "Error returned ?"
    print >> sys.stderr, x


d = deploy_infrastructure_EC2(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id, instance_size,
                             bucket_name, keypair_name, instance_name, admin_privkey_path, 
                             website_pubkey, leastauthority_repo_path, leastauth_commit_ref, 
                             secret_conf_repo_path, secrets_commit_ref, stdout, stderr, clock=None)

d.addCallbacks(printer, eb)
#d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run() 

