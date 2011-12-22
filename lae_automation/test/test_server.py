from cStringIO import StringIO
from twisted.trial.unittest import TestCase

from lae_automation import server
from lae_automation.server import api

class TestServerModule(TestCase):
    def setUp(self):

        self.number_whoamis = 0
        self.number_runs = 0

        def call_api_run(argstring, pty, **kwargs):
            self.failUnlessEqual(self.MOCKFABRICAPIRUNARGSLIST[self.number_runs], (argstring, pty, kwargs))
            self.number_runs = self.number_runs + 1
            if argstring == 'whoami':
                self.number_whoamis = self.number_whoamis + 1
                if self.number_whoamis == 1:
                    return 'ubuntu'
                elif self.number_whoamis == 2:
                    return 'customer'
        self.patch(api, 'run', call_api_run)

        self.apisudocalls = []
        self.number_sudos = 0
        def call_api_sudo(argstring, pty=False, **kwargs):

            self.failUnlessEqual(self.MOCKFABRICAPISUDOARGSLIST[self.number_sudos], (argstring, pty, kwargs))
            self.number_sudos = self.number_sudos + 1
        self.patch(api, 'sudo', call_api_sudo)


        def call_api_reboot(seconds, *args, **kwargs):
            self.failUnlessEqual(seconds, 60)
        self.patch(api, 'reboot', call_api_reboot)

    def test_install_server(self):

        self.MOCKFABRICAPIRUNARGSLIST = [('whoami', False, {}), ('wget https://leastauthority.com/static/patches/txAWS-0.2.1.post2.tar.gz', False, {}), ('tar -xzvf txAWS-0.2.1.post2.tar.gz', False, {}), ('whoami', False, {}), ('rm -rf /home/customer/LAFS_source', False, {}), ('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source', False, {}), ('python ./setup.py build', False, {}), ('mkdir -p introducer storageserver', False, {}), ('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.', False, {}), ('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.', False, {})]




        self.MOCKFABRICAPISUDOARGSLIST = [('apt-get update', False, {}), ('apt-get upgrade -y', False, {}), ('apt-get install -y linux-ec2 linux-image-ec2', False, {}), ('apt-get install -y python-dev', False, {}), ('apt-get install -y python-setuptools', False, {}), ('apt-get install -y exim4-base', False, {}), ('apt-get install -y darcs', False, {}), ('easy_install foolscap', False, {}), ('python ./setup.py install', False, {}), ('adduser --disabled-password --gecos "" customer || echo Assuming that user already exists.', False, {}), ('mkdir -p /home/customer/.ssh/', False, {}), ('chown customer:customer /home/customer/.ssh', False, {}), ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}), ('chown customer:customer /home/customer/.ssh/authorized_keys', False, {}), ('chmod 400 /home/customer/.ssh/authorized_keys', False, {}), ('chmod 700 /home/customer/.ssh/', False, {})]

        MHOSTNAME = '0.0.0.0'
        MKEYFILENAME = 'EC2MOCKKEYFILENAME.pem'
        stdout = StringIO()
        stderr = StringIO()
        server.install_server(MHOSTNAME, MKEYFILENAME, stdout, stderr)
        print self.apisudocalls
