
import mock

from cStringIO import StringIO
from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from datetime import datetime

from lae_automation import server
from lae_automation.server import api


INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

# Vector data for the config file data:
from lae_automation.test.test_vectors import MOCKJSONCONFIGFILE
CONFIGFILEJSON = MOCKJSONCONFIGFILE
def fifo(xs):
    xs.reverse()
    return xs


class CommonTestFixture(TestCase):
    def setUp(self):
        self.TEST_SHA1_STRING = 'deadbeef'*5
        self.TEST_IPv4_STRING = '0.0.0.0'
        self.TEST_LOCAL_HOST_STRING = self.TEST_IPv4_STRING
        self.TEST_LOCAL_REPO_STRING = './.git'
        self.TEST_ADMIN_PRIVKEY_PATH_STRING = '../secret_config/PRIVKEYFILE.pem'
        self.TEST_GIT_SSH_PATH_STRING = 'PATHTOLADIR/leastauthority.com/git_ssh.sh'
        self.TEST_LIVE_PATH_STRING = '/home/website/SECRET_OR_LA_DIR/.git'
        self.TEST_DATETIME = datetime(2014, 5, 19, 14, 32, 31, 788921)
        self.TEST_TAG_NAME = '2014-05-19T14-32-31Z_0.0.0.0_deadbeef'


class TestLocalGitOperations(CommonTestFixture):
    def setUp(self):
        super(TestLocalGitOperations, self).setUp()
        self.TEST_RUN_GIT_ARGSTRING = 'TESTGITCOMMAND'

        self.MOCK_COMMAND_LIST = mock.Mock()
        self.MOCK_CALL_RUN = mock.Mock()
        def call_utcnow():
            return self.TEST_DATETIME
        self.patch(server, 'utcnow', call_utcnow)

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
        self.failUnlessEqual(unique_tag_name, self.TEST_TAG_NAME)

    def test_tag_local_repo(self):
        unique_tag_name = server.tag_local_repo(self.TEST_IPv4_STRING, self.TEST_LOCAL_REPO_STRING,
                                                self.TEST_SHA1_STRING)
        self.failUnlessEqual(unique_tag_name, self.TEST_TAG_NAME)
        self.failUnlessEqual(self.MOCK_COMMAND_LIST.call_args[0][0], ['/usr/bin/git',
                                                      '--git-dir=./.git',
                                                      'tag',
                                                      self.TEST_TAG_NAME,
                                                      'deadbeefdeadbeefdeadbeefdeadbeefdeadbeef'])


from lae_automation.server import PathFormatError
class TestSetupGitBadPaths(CommonTestFixture):
    def setUp(self):
        super(TestSetupGitBadPaths,self).setUp()

    def test_setup_git_deploy_relative_path(self):
        self.TEST_LIVE_PATH_STRING = 'oops'
        self.failUnlessRaises(PathFormatError, server.setup_git_deploy,
                              self.TEST_IPv4_STRING, self.TEST_ADMIN_PRIVKEY_PATH_STRING,
                              self.TEST_GIT_SSH_PATH_STRING, self.TEST_LIVE_PATH_STRING,
                              self.TEST_LOCAL_REPO_STRING, self.TEST_SHA1_STRING)

    def test_setup_git_deploy_terminal_slash_path(self):
        self.TEST_LIVE_PATH_STRING = 'oops'
        self.failUnlessRaises(PathFormatError, server.setup_git_deploy,
                              self.TEST_IPv4_STRING, self.TEST_ADMIN_PRIVKEY_PATH_STRING,
                              self.TEST_GIT_SSH_PATH_STRING, self.TEST_LIVE_PATH_STRING,
                              self.TEST_LOCAL_REPO_STRING, self.TEST_SHA1_STRING)


class TestSetupGitValidPaths(CommonTestFixture):
    def setUp(self):
        super(TestSetupGitValidPaths,self).setUp()
        self.TEST_LIVE_PATH_STRING = '/legitimate_path'

        self.MOCK_CALL_SHELL_QUOTE = mock.Mock(name='shell_quote')
        def call_shell_quote(path):
            # We're not testing shell_quote so we'll use test vectors that don't
            # need special quote handling.
            self.MOCK_CALL_SHELL_QUOTE(path)
            return path
        self.patch(server, 'shell_quote', call_shell_quote)

        self.MOCK_CALL_RUN = mock.Mock(name='run')
        def call_run(run_arg_string):
            return self.MOCK_CALL_RUN(run_arg_string)
        self.patch(server, 'run', call_run)

        self.MOCK_CALL_RUN_GIT = mock.Mock(name='run_git')
        def call_run_git(run_git_args):
            return self.MOCK_CALL_RUN_GIT(run_git_args)
        self.patch(server, 'run_git', call_run_git)

        self.MOCK_CALL_WRITE = mock.Mock(name='write')
        def call_write(string_to_write, destination):
            return self.MOCK_CALL_WRITE(string_to_write, destination)
        self.patch(server, 'write', call_write)

        self.MOCK_CALL_TAG_LOCAL_REPO = mock.Mock(name='tag_local_repo')
        def call_tag_local_repo(host_IP_address, local_repo_path, src_ref_SHA1):
            self.MOCK_CALL_TAG_LOCAL_REPO(host_IP_address, local_repo_path, src_ref_SHA1)
            return self.TEST_TAG_NAME
        self.patch(server, 'tag_local_repo', call_tag_local_repo)

        import os
        os.environ = {}

        self.MOCK_CALL_CHECK_CALL = mock.Mock(name='check_call')
        def call_check_call(command_list, env={}):
            return self.MOCK_CALL_CHECK_CALL(command_list, env)
        self.patch(server.subprocess, 'check_call', call_check_call)


    def test_calls(self):
        server.setup_git_deploy(self.TEST_IPv4_STRING, self.TEST_ADMIN_PRIVKEY_PATH_STRING,
                                self.TEST_GIT_SSH_PATH_STRING, self.TEST_LIVE_PATH_STRING,
                                self.TEST_LOCAL_REPO_STRING, self.TEST_SHA1_STRING)

        self.failUnlessEquals(self.MOCK_CALL_SHELL_QUOTE.call_args_list,
                              [[('/legitimate_path',)],
                               [('/legitimate_path/.git/hooks/post-update',)],
                               [('2014-05-19T14-32-31Z_0.0.0.0_deadbeef',)]
                               ])

        #test_run_calls
        self.failUnlessEquals(self.MOCK_CALL_RUN.call_args_list,
                              [[('rm -rf /legitimate_path',)],
                               [('chmod -f +x /legitimate_path/.git/hooks/post-update',)]
                               ])

        #test_run_git_calls
        self.failUnlessEquals(self.MOCK_CALL_RUN_GIT.call_args_list,
                              [[('init /legitimate_path',)],
                               [('checkout 2014-05-19T14-32-31Z_0.0.0.0_deadbeef',)],
                               [('checkout -b 2014-05-19T14-32-31Z_0.0.0.0_deadbeef',)]
                               ])

        #test_write_call
        self.failUnlessEquals(self.MOCK_CALL_WRITE.call_args_list,
                              [[('#!/bin/bash\ncd /legitimate_path || exit\nunset GIT_DIR\nexec git update-server-info\n',
                                 '/legitimate_path/.git/hooks/post-update',)]
                               ])

        #test_tag_local_repo_call
        self.failUnlessEquals(self.MOCK_CALL_WRITE.call_args_list,
                              [[('#!/bin/bash\ncd /legitimate_path || exit\nunset GIT_DIR\nexec git update-server-info\n',
                                 '/legitimate_path/.git/hooks/post-update',)]
                               ])

        #test_check_call_call
        self.failUnlessEquals(self.MOCK_CALL_CHECK_CALL.call_args_list[0][0][0],
                              ['/usr/bin/git', '--git-dir=./.git', 'push',
                               'website@0.0.0.0:/legitimate_path',
                               '2014-05-19T14-32-31Z_0.0.0.0_deadbeef:2014-05-19T14-32-31Z_0.0.0.0_deadbeef'])
        self.failUnlessEquals(self.MOCK_CALL_CHECK_CALL.call_args_list[0][0][1],
                              {'GIT_SSH': 'PATHTOLADIR/leastauthority.com/git_ssh.sh',
                               'PRIVATE_KEY': '../secret_config/PRIVKEYFILE.pem'})


class TestUnattendedServerUpgrade(CommonTestFixture):
    def setUp(self):
        super(TestUnattendedServerUpgrade, self).setUp()
        self.TEST_REBOOT_SECONDS = 300
        self.MOCK_API = mock.Mock()

        self.MOCK_CALL_SUDO = mock.Mock(name='sudo')
        def call_sudo(command):
            self.MOCK_CALL_SUDO(command)
        self.patch(server, 'sudo', call_sudo)

        self.MOCK_CALL_API_REBOOT = mock.Mock(name='reboot')
        def call_reboot(seconds):
            self.MOCK_CALL_API_REBOOT(seconds)
        self.patch(server.api, 'reboot', call_reboot)

        self.MOCK_CALL_SUDO_APT_GET = mock.Mock(name='sudo_apt_get')
        def call_sudo_apt_get(command):
            self.MOCK_CALL_SUDO_APT_GET(command)
        self.patch(server, 'sudo_apt_get', call_sudo_apt_get)


        server.run_unattended_upgrade(self.MOCK_API, self.TEST_REBOOT_SECONDS)

    def test_sudo_apt_get_call(self):
        self.failUnlessEqual(self.MOCK_CALL_SUDO_APT_GET.call_args_list, [[('update',)]])

    def test_sudo_call(self):
        self.failUnlessEqual(self.MOCK_CALL_SUDO.call_args_list, [[('unattended-upgrade --minimal_upgrade_steps',)]])

    def test_api_reboot_call(self):
        self.failUnlessEqual(self.MOCK_API.reboot.call_args_list, [[(300,)]])


class TestInstallInfrastructureServer(CommonTestFixture):
    def setUp(self):
        super(TestInstallInfrastructureServer, self).setUp()
        self.MOCK_CALL_RUN_UNATTENDED_UPGRADE = mock.Mock(name='run_unattended_upgrade')
        def call_run_unattended_upgrade():
            self.MOCK_CALL_RUN_UNATTENDED_UPGRADE()
        self.patch(server, 'run_unattended_upgrade', call_run_unattended_upgrade)
        #server.install_infrastructure_server(self.TEST_LOCAL_HOST_STRING,
        #                                     self.TEST_ADMIN_PRIVKEY_PATH_STRING)

    #def test_apt_unattended_upgrade(self):
        #self.failUnlessEqual(self.MOCK_CALL_RUN_UNATTENDED_UPGRADE.call_args_list,
        #                     [('unattended-upgrade --minimal_upgrade_steps',)])



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
        self.WHOAMI_FIFO = fifo(['ubuntu', 'ubuntu', 'monitor', 'customer'])
        self.RUNARGS_FIFO = fifo([
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('wget https://tahoe-lafs.org/source/tahoe-lafs/deps/tahoe-lafs-dep-sdists/txAWS-0.2.1.post5.tar.gz', False, {}),
            ('tar -xzvf txAWS-0.2.1.post5.tar.gz', False, {}),
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('rm -rf /home/customer/LAFS_source', False, {}),
            ('git clone https://github.com/tahoe-lafs/tahoe-lafs.git LAFS_source', False, {}),
            ('git checkout 2237-cloud-backend-s4', False, {}),
            ('python ./setup.py build', False, {}),
            ('mkdir -p introducer storageserver', False, {}),
            ('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.', False, {}),
            ('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.', False, {})
        ])
        self.SUDOARGS_FIFO = fifo([
            ('apt-get update', False, {}),
            ('apt-get -y install python-dev', False, {}),
            ('apt-get -y install python-pip', False, {}),
            ('apt-get -y install git-core', False, {}),
            ('apt-get -y install libffi6', False, {}),
            ('apt-get -y install openssl', False, {}),
            ('apt-get -y install libssl1.0.0', False, {}),
            ('apt-get -y install python-nevow', False, {}),
            ('apt-get -y install python-crypto', False, {}),
            ('apt-get -y install python-dateutil', False, {}),
            ('apt-get -y install python-foolscap', False, {}),
            ('apt-get -y install python-six', False, {}),
            ('apt-get -y install python-pycparser', False, {}),
            ('apt-get -y install python-unidecode', False, {}),
            ('apt-get -y install python-zfec', False, {}),
            ('apt-get -y install python-simplejson', False, {}),
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
            if argstring == 'cat /home/customer/introducer/private/introducer.furl':
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
        MSECRETSFP = FilePath('test_bounce_server_secrets')
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
                ('rm -f /home/customer/introducer/introducer.furl'
                      ' /home/customer/introducer/private/introducer.furl'
                      ' /home/customer/introducer/logport.furl', False, {}),
                ('LAFS_source/bin/tahoe restart introducer && sleep 5', False, {}),
                ('cat /home/customer/introducer/private/introducer.furl', False, {}),
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
                             STDOUT, STDERR, MSECRETSFP, self.CONFIGFILEPATH)
        self.failUnless(MSECRETSFP.exists())
        MSECRETSFP.remove()
        self._check_all_done()

