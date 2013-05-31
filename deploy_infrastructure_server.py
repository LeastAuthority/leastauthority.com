#!/usr/bin/env python
"""This script launches a new EC2 instance, and provisions it with a version of the Least Authority code base.  Finally it launches the webserver specified in that code.

Configuration Options:
  To select a particular version of the Least Authority code base, one can specify a git commit SHA1 by assigning the default encoded SHA1 to the variable "COMMITSHA1".  This functionality is not yet implemented.XXX
  AWS credentials, ami parameters (e.g. size and ami id) and metadata to associate with the instance (e.g. instance name) are parsed from the lae_automation_config.json configuration file.
  The "source_git_directory" must be the absolute path (as a string) of a git repository containing the Least Authority code.

"""
import sys, os #We use sys to pass information to sys.std{out,err}. We use os to guarantee termination of the reactor
from lae_automation.config import DeployConfig #This class is used to parse lae_automation_config.json
from twisted.python.filepath import FilePath #Provides relatively atomic operations on files and OO interface to same
from lae_automation.initialize import deploy_infrastructure_EC2 #This function (via deploy_EC2_instance -->txaws) interfaces with AWS and causes it to spin up a new EC2! After launch it invokes 'verify_and_store_serverssh_pubkey. 
from lae_automation.server import install_infrastructure_server #Now that we have a verified EC2, provision it with the Least Authority code.
from lae_automation.signup import EC2_ENDPOINT #Where in the AWS cloud do we do this? EC2_ENDPOINT.

COMMITSHA1 = "SHA1commithash" # This will change to a commit hash. As a stub I've used seconds since the Unix epoch as reported by XXX.
configpath='../lae_automation_config.json' #Contains config information.

config = DeployConfig(COMMITSHA1, sys.argv[1], configpath) #sys.argv[1] 'testing' is used to index which 'type' of code to provision perhaps it will be extended to take developer names, or to provision specialized EC2s (e.g. infrastructure vs SSEC2), or perhaps it's unnecessary complexity.

#Configuration copied from most recent product
ami_image_id = str(config.products[-1]['ami_image_id']) #https://en.wikipedia.org/wiki/Amazon_Machine_Image maybe someday we'll cook up our own!
instancesize = str(config.products[-1]['instance_size']) #quite small ~.5 GiB of RAM

#Configuration which is specific to the test account
ec2accesskeyid = str(config.deployment[COMMITSHA1]['ec2_access_key_id']) #A nonsecret index AWS uses to look up our secret key. (See ec2secretkey.)
admin_keypair_name = str(config.deployment[COMMITSHA1]['testing_keypair_name']) #The name of the ssh key we want AWS to set for the "ubuntu" account on the EC2
admin_privkey_path = str(config.deployment[COMMITSHA1]['testing_privkey_path']) #Where we store the ssh privkey for "ubuntu".  Of course this is a secret knowledge of which gives root access to the EC2 (over ssh).
ec2secretpath = str(config.deployment[COMMITSHA1]['ec2_test_secret_path']) # Path of the secret (see below)
instancename = str(config.deployment[COMMITSHA1]['instance_name']) #a metadata tag associated with the instance.

ec2secretkey = FilePath(ec2secretpath).getContent().strip() #The secret we share with AWS that let's us sign AWS REST Query requests. Knowledge of this secret allows Least Authority authenticated requests to AWS.

#source_git_directory = '/home/backup/disasterrecovery/' #Currently hardcoded (obviously!). This is the repo that will provide code to provision the EC2 with a Least Authority infrastructure server.
source_git_directory = '/home/arc/leastauthorityenchilada' #Currently hardcoded (obviously!). This is the repo that will provide code to provision the EC2 with a Least Authority infrastructure server.

def printer(x):
    """This handy function let's us see what's going on between calls in the callback chain."""
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    """This handy function let's us see what's going on between errors in the callback chain."""
    print >> sys.stderr, "Error returned ?"
    print >> sys.stderr, x

# "d" is a deferred. The code in this module doesn't actually need to be "reactive" interfaces to Least Authority Code assume a reactive framework, so we produce reactive scripts, to use the interface.

d = deploy_infrastructure_EC2(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, ami_image_id, instancesize, 'infrastructure', admin_keypair_name, 'infrastructure', sys.stdout, sys.stderr)
    
d.addCallbacks(printer, eb)
d.addCallbacks(lambda IP_from_verification: install_infrastructure_server(IP_from_verification, admin_privkey_path, source_git_directory, COMMITSHA1, sys.stdout, sys.stderr), eb)
d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor
# Because we're being reactive we need a reactor
# 
reactor.run() # http://krondo.com/?p=1209

