
import mock

from cStringIO import StringIO
from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_automation import server
from lae_automation.server import api


INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

# Vector data for the config file data:
CONFIGFILEJSON = """{
  "products": [
    { "full_name":        "The test vector product.",
      "product_code":     "ABCDEFGH",
      "product_token":    "{ProductToken}TESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
      "ami_image_id":     "ami-testfbc2",
      "instance_size":    "t1.testy"
    }
  ],
  "ec2_access_key_id":    "TESTAAAAAAAAAAAAAAAA",
  "admin_keypair_name":   "ADMINKEYS",
  "admin_privkey_path":   "ADMINKEYS.pem",
  "monitor_pubkey_path":  "MONITORKEYS.pub",
  "monitor_privkey_path": "MONITORKEYS.pem",
  "incident_gatherer_furl": "MOCK_incident_gatherer_furl",
  "stats_gatherer_furl":    "MOCK_stats_gatherer_furl"
}"""

def fifo(xs):
    xs.reverse()
    return xs

class TestGitBase(TestCase):
    def setUp(self):
        self.TEST_SHA1_STRING = 'deadbeef'*5
        self.TEST_IPv4_STRING = '0.0.0.0'
        self.TEST_LOCAL_REPO_STRING = './.git'

class TestLocalGitOperations(TestGitBase):
    def setUp(self):
        super(TestLocalGitOperations, self).setUp()
        self.TEST_SSE_STRING = '1399917193'
        self.TEST_RUN_GIT_ARGSTRING = 'TESTGITCOMMAND'

        self.MOCK_COMMAND_LIST = mock.Mock()
        self.MOCK_CALL_RUN = mock.Mock()
        def call_time():
            return self.TEST_SSE_STRING
        self.patch(server.time, 'time', call_time)

        def call_check_call(command_list):
            self.MOCK_COMMAND_LIST(command_list)
        self.patch(server.subprocess, 'check_call', call_check_call)

        def call_run(run_string):
            self.MOCK_CALL_RUN(run_string)
            self.MOCK_CALL_RUN.succeeded = True
            self.MOCK_CALL_RUN.failed = False
            return self.MOCK_CALL_RUN

        self.patch(server, 'run', call_run)


    def test_run_git(self):
        result = server.run_git(self.TEST_RUN_GIT_ARGSTRING)
        self.failUnlessEqual(result.call_args[0][0], '/usr/bin/git TESTGITCOMMAND')

    def test_make_unique_tag_name(self):
        unique_tag_name = server.make_unique_tag_name(self.TEST_IPv4_STRING, self.TEST_SHA1_STRING)
        self.failUnlessEqual(unique_tag_name, '1399917193_0.0.0.0_deadbeef')

    def test_tag_local_repo(self):
        unique_tag_name = server.tag_local_repo(self.TEST_IPv4_STRING, self.TEST_LOCAL_REPO_STRING,
                                                self.TEST_SHA1_STRING)
        self.failUnlessEqual(unique_tag_name, '1399917193_0.0.0.0_deadbeef')
        self.failUnlessEqual(self.MOCK_COMMAND_LIST.call_args[0][0], ['/usr/bin/git',
                                                      '--git-dir=./.git',
                                                      'tag',
                                                      '1399917193_0.0.0.0_deadbeef',
                                                      'deadbeefdeadbeefdeadbeefdeadbeefdeadbeef'])

from lae_automation.server import PathFormatError
class TestRemoteGitOperations(TestGitBase):
    def setUp(self):
        super(TestRemoteGitOperations,self).setUp()
        self.TEST_ADMIN_PRIVKEY_PATH_STRING = '../secret_config/PRIVKEYFILE.pem'
        self.TEST_GIT_SSH_PATH_STRING = 'PATHTOLADIR/leastauthority.com/git_ssh.sh'
        self.TEST_LIVE_PATH_STRING = '/home/website/SECRET_OR_LA_DIRo/.git'

    def test_setup_git_deploy_relative_path(self):
        self.TEST_LIVE_PATH_STRING = 'oops/'
        self.failUnlessRaises(PathFormatError, server.setup_git_deploy,
                              self.TEST_IPv4_STRING, self.TEST_ADMIN_PRIVKEY_PATH_STRING,
                              self.TEST_GIT_SSH_PATH_STRING, self.TEST_LIVE_PATH_STRING,
                              self.TEST_LOCAL_REPO_STRING, self.TEST_SHA1_STRING)

    #def test_setup_git_deploy_notabs_path(self):
    #    self.TEST_LIVE_PATH_STRING = 'oops'
    #    server.setup_git_deploy(self.TEST_IPv4_STRING, self.TEST_ADMIN_PRIVKEY_PATH_STRING,
    #                            self.TEST_GIT_SSH_PATH_STRING, self.TEST_LIVE_PATH_STRING,
    #                            self.TEST_LOCAL_REPO_STRING, self.TEST_SHA1_STRING)


class TestServerModule(TestCase):
    def setUp(self):
        self.WHOAMI_FIFO = []
        self.RUNARGS_FIFO = []
        self.SUDOARGS_FIFO = []
        self.WRITEARGS_FIFO = []
        def call_api_run(argstring, pty, **kwargs):
            self.failUnlessEqual(self.RUNARGS_FIFO.pop(), (argstring, pty, kwargs))
            if argstring == 'whoami':
                return self.WHOAMI_FIFO.pop()
        self.patch(api, 'run', call_api_run)

        def call_api_sudo(argstring, pty=False, **kwargs):
            self.failUnlessEqual(self.SUDOARGS_FIFO.pop(), (argstring, pty, kwargs))
        self.patch(api, 'sudo', call_api_sudo)

        def call_api_reboot(seconds, *args, **kwargs):
            self.failUnlessEqual(seconds, 240)
        self.patch(api, 'reboot', call_api_reboot)

        def call_write(value, remote_path, use_sudo=False, mode=None):
            self.failUnlessEqual(self.WRITEARGS_FIFO.pop(), (value, remote_path, use_sudo, mode))
            return [remote_path]
        self.patch(server, 'write', call_write)

        self.CONFIGFILEPATH = 'init_test_config.json'
        FilePath(self.CONFIGFILEPATH).setContent(CONFIGFILEJSON)

    def _check_all_done(self):
        self.failUnlessEqual(self.WHOAMI_FIFO, [])
        self.failUnlessEqual(self.RUNARGS_FIFO, [])
        self.failUnlessEqual(self.SUDOARGS_FIFO, [])
        self.failUnlessEqual(self.WRITEARGS_FIFO, [])

    def tearDown(self):
        FilePath(self.CONFIGFILEPATH).remove()

    def test_install_server(self):
        self.WHOAMI_FIFO = fifo(['ubuntu', 'monitor', 'customer'])
        self.RUNARGS_FIFO = fifo([
            ('whoami', False, {}),
            ('wget https://tahoe-lafs.org/source/tahoe-lafs/deps/tahoe-lafs-dep-sdists/txAWS-0.2.1.post5.tar.gz', False, {}),
            ('tar -xzvf txAWS-0.2.1.post5.tar.gz', False, {}),
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('rm -rf /home/customer/LAFS_source', False, {}),
            ('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source', False, {}),
            ('python ./setup.py build', False, {}),
            ('mkdir -p introducer storageserver', False, {}),
            ('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.', False, {}),
            ('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.', False, {})
        ])
        self.SUDOARGS_FIFO = fifo([
            ('apt-get update', False, {}),
            ('apt-get -y dist-upgrade', False, {}),
            ('apt-get -y install python-dev', False, {}),
            ('apt-get -y install python-setuptools', False, {}),
            ('apt-get -y install exim4-base', False, {}),
            ('apt-get -y install darcs', False, {}),
            ('apt-get -y install python-foolscap', False, {}),
            ('apt-get -y remove --purge whoopsie', False, {}),
            ('python ./setup.py install', False, {}),
            ('adduser --disabled-password --gecos "" customer || echo Assuming that customer already exists.', False, {}),
            ('mkdir -p /home/customer/.ssh/', False, {}),
            ('chown customer:customer /home/customer/.ssh', False, {}),
            ('chmod -f u+w /home/customer/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.', False, {}),
            ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}),
            ('chown customer:customer /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod -f 400 /home/customer/.ssh/authorized_keys', False, {}),
            ('chmod -f 700 /home/customer/.ssh/', False, {}),
            ('adduser --disabled-password --gecos "" monitor || echo Assuming that monitor already exists.', False, {}),
            ('mkdir -p /home/monitor/.ssh/', False, {}),
            ('chown monitor:monitor /home/monitor/.ssh', False, {}),
            ('chmod -f u+w /home/monitor/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.', False, {}),
            ('chown monitor:monitor /home/monitor/.ssh/authorized_keys', False, {}),
            ('chmod -f 400 /home/monitor/.ssh/authorized_keys', False, {}),
            ('chmod -f 700 /home/monitor/.ssh/', False, {})
        ])
        self.WRITEARGS_FIFO = fifo([('THIS IS A MOCK PUBLIC KEY', '/home/monitor/.ssh/authorized_keys', True, None)])

        MHOSTNAME = '0.0.0.0'
        ADMINPRIVKEYPATH = 'mockEC2adminkeys.pem'
        MONITORPUBKEY = 'THIS IS A MOCK PUBLIC KEY'
        MONITORPRIVKEYPATH = 'mockEC2monitorkeys.pem'
        STDOUT = StringIO()
        STDERR = StringIO()

        server.install_server(MHOSTNAME, ADMINPRIVKEYPATH, MONITORPUBKEY, MONITORPRIVKEYPATH, STDOUT, STDERR)
        self._check_all_done()


    def test_create_account(self):
        ACCOUNT_NAMES_AND_KEYS = [('customer', None),
                                  ('monitor', 'MONSSHPUBKEY')]
        STDOUT = StringIO()
        STDERR = StringIO()

        for (acct_name, pubkey) in ACCOUNT_NAMES_AND_KEYS:
            self.WHOAMI_FIFO = fifo([])
            self.RUNARGS_FIFO = fifo([])
            SUDOARGS = [
                ('adduser --disabled-password --gecos "" %s || echo Assuming that %s already exists.' % (2*(acct_name,)), False, {}),
                ('mkdir -p /home/%s/.ssh/' % acct_name, False, {}),
                ('chown %s:%s /home/%s/.ssh' % (3*(acct_name,)), False, {}),
                ('chmod -f u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (acct_name,), False, {}),
                ('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(acct_name,)), False, {}),
                ('chmod -f 400 /home/%s/.ssh/authorized_keys' % acct_name, False, {}),
                ('chmod -f 700 /home/%s/.ssh/' % acct_name, False, {})]
            if pubkey is None:
                SUDOARGS.insert(4, ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}))
            self.SUDOARGS_FIFO = fifo(SUDOARGS)
            if pubkey is None:
                self.WRITEARGS_FIFO = fifo([])
            else:
                self.WRITEARGS_FIFO = fifo([(pubkey, '/home/%s/.ssh/authorized_keys' % acct_name, True, None)])

            server.create_account(acct_name, pubkey, STDOUT, STDERR)
        self._check_all_done()


    def test_bounce_server(self):
        def call_set_host_and_key(publichost, admin_privkey_path, username):
            self.failUnlessEqual(publichost, '0.0.0.0')
            self.failUnlessEqual(admin_privkey_path, 'mockEC2adminkeys.pem')
            self.failUnlessEqual(username, 'customer')
        self.patch(server, 'set_host_and_key', call_set_host_and_key)
        def call_api_run(argstring, pty, **kwargs):
            self.failUnlessEqual(self.RUNARGS_FIFO.pop(), (argstring, pty, kwargs))
            if argstring == 'whoami':
                return self.WHOAMI_FIFO.pop()
            if argstring == 'cat /home/customer/introducer/introducer.furl':
                return INTERNALINTROFURL
        self.patch(api, 'run', call_api_run)
        MHOSTNAME = '0.0.0.0'
        ADMINPRIVKEYPATH = 'mockEC2adminkeys.pem'
        MPRIVHOST = '1.1.1.1'
        ACCESSKEYID = 'TEST'+'A'*16
        SECRETACCESSKEY = 'TEST'+'A'*36
        MUSERTOKEN = None
        MPRODUCTTOKEN = None
        BUCKETNAME = 'foooooo'
        STDOUT = StringIO()
        STDERR = StringIO()
        MSECRETSFILE = StringIO()
        INTERNALINTROFURL = 'pb://TUBID@LOCATION/SWISSNUM'
        from lae_automation.server import TAHOE_CFG_TEMPLATE
        from lae_automation.server import RESTART_SCRIPT
        test_tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': BUCKETNAME,
                                               'publichost': MHOSTNAME,
                                               'privatehost': MPRIVHOST,
                                               'introducer_furl': INTERNALINTROFURL,
                                               's3_access_key_id': ACCESSKEYID,
                                               'bucket_name': BUCKETNAME,
                                               'incident_gatherer_furl': "MOCK_incident_gatherer_furl",
                                               'stats_gatherer_furl': "MOCK_stats_gatherer_furl"}
        self.WHOAMI_FIFO = []
        self.RUNARGS_FIFO = fifo([
                ('rm -f /home/customer/introducer/introducer.furl /home/customer/introducer/logport.furl', False, {}),
                ('LAFS_source/bin/tahoe restart introducer && sleep 5', False, {}),
                ('cat /home/customer/introducer/introducer.furl', False, {}),
                ('chmod -f u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.', False, {}),
                ('LAFS_source/bin/tahoe restart storageserver && sleep 5', False, {}),
                ('ps -fC tahoe', False, {}),
                ('netstat -atW', False, {}),
                ('crontab /home/customer/ctab', False, {}),
                ('cat /home/customer/introducer/private/node.pem', False, {}),
                ('cat /home/customer/introducer/my_nodeid', False, {}),
                ('cat /home/customer/storageserver/private/node.pem', False, {}),
                ('cat /home/customer/storageserver/my_nodeid', False, {}),
                ('if [[ -e /home/customer/storageserver/private/node.privkey ]]; then cat /home/customer/storageserver/private/node.privkey; fi', False, {}),
                ])
        self.SUDOARGS_FIFO = []
        self.WRITEARGS_FIFO = fifo([
                (INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port', False, None),
                (SERVER_PORT + '\n', '/home/customer/storageserver/client.port', False, None),
                (test_tahoe_cfg, '/home/customer/storageserver/tahoe.cfg', False, None),
                (SECRETACCESSKEY, '/home/customer/storageserver/private/s3secret', False, 0640),
                (RESTART_SCRIPT, '/home/customer/restart.sh', False, 0750),
                ('@reboot /home/customer/restart.sh\n', '/home/customer/ctab', False, None)
                ])
        server.bounce_server(MHOSTNAME, ADMINPRIVKEYPATH, MPRIVHOST, ACCESSKEYID, SECRETACCESSKEY, \
                             MUSERTOKEN, MPRODUCTTOKEN, BUCKETNAME, None, \
                             STDOUT, STDERR, MSECRETSFILE, self.CONFIGFILEPATH)
        self._check_all_done()

