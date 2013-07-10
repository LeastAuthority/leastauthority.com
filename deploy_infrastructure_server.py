#!/usr/bin/env python
"""This script:
(1) launches a new EC2 instance
(2) provisions it with a version of the Least Authority code base
(3) launches the webserver specified in that code.

Configuration Options:
  To select a particular version of the Least Authority code base, one can specify a reference".  

  AWS credentials, ami parameters (e.g. size and ami id) and metadata to associate with the instance 
(e.g. instance name) are parsed from the lae_automation_config.json configuration file.
  The "source_git_directory" must be the absolute path (as a string) of a git repository containing 
the Least Authority code.

"""
import sys, os, argparse 
from lae_automation.config import Config
from twisted.python.filepath import FilePath
from lae_automation.initialize import deploy_infrastructure_EC2 
from lae_automation.signup import EC2_ENDPOINT

COMMIT_TAG = sys.argv[1] 
configpath='../secret_config/lae_automation_config.json'

config = Config(configpath)

#Configuration copied from most recent product
#https://en.wikipedia.org/wiki/Amazon_Machine_Image
ami_image_id = str(config.products[-1]['ami_image_id']) 

instancesize = str(config.products[-1]['instance_size'])
ec2accesskeyid = str(config.other['ec2_access_key_id'])
admin_keypair_name = str(config.other['admin_keypair_name'])
admin_privkey_path = str(config.other['admin_privkey_path'])
ec2secretpath = str(config.other['ec2_secret_path'])

print config.other['deployment']
#Configuration which is specific to the test account

instancename = str(config.other['deployment'][COMMIT_TAG]['instance_name'])

ec2secretkey = FilePath(ec2secretpath).getContent().strip()

source_git_directory = '/home/production_backup/disasterrecovery/.git'

def printer(x):
    """This handy function let's us see what's going on between calls in the callback chain."""
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    """This handy function let's us see what's going on between errors in the callback chain."""
    print >> sys.stderr, "Error returned ?"
    print >> sys.stderr, x


d= deploy_infrastructure_EC2(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id, instance_size,
                              bucket_name, keypair_name, instance_name, admin_privkey_path, 
                              website_pubkey, leastauth_repo, la_commit_hash, secretconf_repo,
                              sc_commit_hash, stdout, stderr, clock=None):

d.addCallbacks(printer, eb)
#d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run() 

