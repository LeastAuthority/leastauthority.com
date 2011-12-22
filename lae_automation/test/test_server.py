from cStringIO import StringIO
from twisted.trial.unittest import TestCase

from lae_automation import server
from lae_automation.server import api

import sys
class TestServerModule(TestCase):
    def setUp(self):

        self.number_whoamis = 0
        self.number_runs = 0

        def call_api_run(argstring, pty, **kwargs):
            self.failUnlessEqual(self.RUNARGSLIST[self.number_runs], (argstring, pty, kwargs))
            self.number_runs = self.number_runs + 1
            #self.RUNARGSLIST.append((argstring, pty, kwargs))
            if argstring == 'whoami':
                self.number_whoamis = self.number_whoamis + 1
                if self.number_whoamis == 1:
                    return 'ubuntu'
                elif self.number_whoamis == 2:
                    return 'customer'
        self.patch(api, 'run', call_api_run)

        self.number_sudos = 0
        def call_api_sudo(argstring, pty=False, **kwargs):
            #self.SUDOARGSLIST.append((argstring, pty, kwargs))
            self.failUnlessEqual(self.SUDOARGSLIST[self.number_sudos], (argstring, pty, kwargs))
            self.number_sudos = self.number_sudos + 1
        self.patch(api, 'sudo', call_api_sudo)

        def call_api_reboot(seconds, *args, **kwargs):
            self.failUnlessEqual(seconds, 60)
        self.patch(api, 'reboot', call_api_reboot)


    def test_install_server(self):
        self.RUNARGSLIST = [\
            ('whoami', False, {}),
            ('wget https://leastauthority.com/static/patches/txAWS-0.2.1.post2.tar.gz', False, {}),
            ('tar -xzvf txAWS-0.2.1.post2.tar.gz', False, {}),
            ('whoami', False, {}),
            ('rm -rf /home/customer/LAFS_source', False, {}),
            ('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source', False, {}),
            ('python ./setup.py build', False, {}),
            ('mkdir -p introducer storageserver', False, {}),
            ('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.', False, {}),
            ('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.', False, {})]

        self.SUDOARGSLIST = [\
            ('apt-get update', False, {}),
            ('apt-get upgrade -y', False, {}),
            ('apt-get install -y linux-ec2 linux-image-ec2', False, {}),
            ('apt-get install -y python-dev', False, {}),
            ('apt-get install -y python-setuptools', False, {}),
            ('apt-get install -y exim4-base', False, {}),
            ('apt-get install -y darcs', False, {}),
            ('easy_install foolscap', False, {}),
            ('python ./setup.py install', False, {}),
            ('adduser --disabled-password --gecos "" customer || echo Assuming that customer already exists.', False, {}),
            ('mkdir -p /home/customer/.ssh/', False, {}),
            ('chown customer:customer /home/customer/.ssh', False, {}),
            ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}),
            ('chown customer:customer /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 400 /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 700 /home/customer/.ssh/', False, {})]

        MHOSTNAME = '0.0.0.0'
        MKEYFILENAME = 'EC2MOCKKEYFILENAME.pem'
        stdout = StringIO()
        stderr = StringIO()
        server.install_server(MHOSTNAME, MKEYFILENAME, stdout, stderr)

    def test_create_account(self):
        from lae_automation import server
        MOCKACCOUNTNAMES = ['customer', 'monitor']
        MKEYFILENAME = 'account_ssh_pkey_fname'
        STDOUT = sys.stdout
        STDERR = sys.stderr
        self.RUNARGSLIST = []
        self.SUDOARGSLIST =  [\
            ('adduser --disabled-password --gecos "" customer || echo Assuming that customer already exists.', False, {}),
            ('mkdir -p /home/customer/.ssh/', False, {}),
            ('chown customer:customer /home/customer/.ssh', False, {}),
            ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}),
            ('chown customer:customer /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 400 /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 700 /home/customer/.ssh/', False, {})]
        def call_write(remote_path, value, mode=None):
            self.failUnless(remote_path == '/home/customer/.ssh/authorized_keys'\
                                or remote_path == '/home/monitor/.ssh/authorized_keys',\
                                '%r is not %r or %r' %\
                                (remote_path, '/home/customer/.ssh/authorized_keys',\
                                     '/home/monitor/.ssh/authorized_keys'))
            return [remote_path]
        self.patch(server, 'write', call_write)
        server.create_account(MOCKACCOUNTNAMES[0], MKEYFILENAME, STDOUT, STDERR)
        #print "sudo: %s"% self.SUDOARGSLIST
        #print "run: %s" % self.RUNARGSLIST
