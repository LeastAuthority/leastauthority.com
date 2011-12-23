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
            if argstring == 'whoami':
                self.number_whoamis = self.number_whoamis + 1
                if self.number_whoamis == 1:
                    return 'ubuntu'
                elif self.number_whoamis == 2:
                    return 'monitor'
                elif self.number_whoamis == 3:
                    return 'customer'

        self.patch(api, 'run', call_api_run)

        self.number_sudos = 0
        def call_api_sudo(argstring, pty=False, **kwargs):
            self.failUnlessEqual(self.SUDOARGSLIST[self.number_sudos], (argstring, pty, kwargs))
            self.number_sudos = self.number_sudos + 1
        self.patch(api, 'sudo', call_api_sudo)

        def call_api_reboot(seconds, *args, **kwargs):
            self.failUnlessEqual(seconds, 60)
        self.patch(api, 'reboot', call_api_reboot)

        def call_write(remote_path, value, mode=None):
            return [remote_path]
        self.patch(server, 'write', call_write)


    def test_install_server(self):
        self.RUNARGSLIST = [
            ('whoami', False, {}),
            ('wget https://leastauthority.com/static/patches/txAWS-0.2.1.post2.tar.gz', False, {}),
            ('tar -xzvf txAWS-0.2.1.post2.tar.gz', False, {}),
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('rm -rf /home/customer/LAFS_source', False, {}),
            ('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source', False, {}),
            ('python ./setup.py build', False, {}),
            ('mkdir -p introducer storageserver', False, {}),
            ('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.', False, {}),
            ('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.', False, {})]

        self.SUDOARGSLIST = [
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
            ('chmod u+w /home/customer/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.', False, {}),
            ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}),
            ('chown customer:customer /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 400 /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod 700 /home/customer/.ssh/', False, {}),
            ('adduser --disabled-password --gecos "" monitor || echo Assuming that monitor already exists.', False, {}),
            ('mkdir -p /home/monitor/.ssh/', False, {}),
            ('chown monitor:monitor /home/monitor/.ssh', False, {}),
            ('chmod u+w /home/monitor/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.', False, {}),
            ('chown monitor:monitor /home/monitor/.ssh/authorized_keys', False, {}),
            ('chmod 400 /home/monitor/.ssh/authorized_keys', False, {}),
            ('chmod 700 /home/monitor/.ssh/', False, {})]

        MHOSTNAME = '0.0.0.0'
        MKEYFILENAME = 'EC2MOCKKEYFILENAME.pem'
        SSHPUBFNAME = 'MONSSHPUBKEY'
        SSHPRIVFNAME = 'MONSSHPRIVATEKEY THIS IS SOOOOSECRET!'
        stdout = StringIO()
        stderr = StringIO()
        server.install_server(MHOSTNAME, MKEYFILENAME, SSHPUBFNAME, SSHPRIVFNAME, stdout, stderr)

    def test_create_account(self):
        from lae_automation import server
        MOCKACCOUNTNAMES = ['customer', 'monitor']
        MKEYFILENAME = 'account_ssh_pkey_fname'
        STDOUT = sys.stdout
        STDERR = sys.stderr
        for acct_name in MOCKACCOUNTNAMES:
            self.number_runs = 0
            self.number_sudos = 0
            self.RUNARGSLIST = []
            self.SUDOARGSLIST = [
                ('adduser --disabled-password --gecos "" %s || echo Assuming that %s already exists.' % (2*(acct_name,)), False, {}),
                ('mkdir -p /home/%s/.ssh/' % acct_name, False, {}),
                ('chown %s:%s /home/%s/.ssh' % (3*(acct_name,)), False, {}),
                ('chmod u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (acct_name,), False, {}),
                ('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(acct_name,)), False, {}),
                ('chmod 400 /home/%s/.ssh/authorized_keys' % acct_name, False, {}),
                ('chmod 700 /home/%s/.ssh/' % acct_name, False, {})]
            if acct_name is None:
                self.SUDOARGSLIST.insert(2, ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}))
            def call_write(remote_path, value, mode=None):
                self.failUnlessEqual(remote_path, '/home/%s/.ssh/authorized_keys' % acct_name)
                return [remote_path]
            self.patch(server, 'write', call_write)
            server.create_account(acct_name, MKEYFILENAME, STDOUT, STDERR)
