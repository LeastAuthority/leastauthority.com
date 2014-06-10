#!/usr/bin/env python
"""This script:
(1) updates a testing infrastructure-server.
"""

import os, sys, subprocess

from twisted.internet import defer

from lae_automation.signup import EC2_ENDPOINT
from lae_automation.server import update_leastauthority_repo

branch_check_command = ['/usr/bin/git', '--git-dir', '../secret_config/.git', 'branch', '--list', 'testing']
current_branch = subprocess.check_output(branch_check_command).strip()
if current_branch != "* testing":
    print "The testing branch of the secret_config repo must be checked out to run this script."
    sys.exit(1)

leastauthority_repo_path = "./.git"
least_repo_HEAD_command = ['/usr/bin/git', '--git-dir', leastauthority_repo_path, 'rev-parse', 'HEAD']
leastauthority_commit_ref = subprocess.check_output(least_repo_HEAD_command).strip()
print leastauthority_commit_ref

admin_keypair_name = "ec2sshadmin"
admin_privkey_path = "../secret_config/ec2sshadmin.pem"

endpoint_uri = EC2_ENDPOINT
bucket_name = 'dummy'
website_pubkey = None
stdout = sys.stdout
stderr = sys.stderr

def printer(x):
    print "callBack return value 'x' is %s" % (x,)
    return x

def eb(x):
    print >> sys.stderr, "Error returned ?"
    print >> sys.stderr, x


d = defer.succeed(update_leastauthority_repo("107.21.225.70", leastauthority_repo_path,
                                             leastauthority_commit_ref, admin_privkey_path))

d.addCallbacks(printer, eb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
from twisted.internet import reactor

reactor.run()

