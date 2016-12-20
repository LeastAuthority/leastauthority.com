
import mock
from hypothesis import given, settings

from os import fdopen
from json import loads, dumps
from cStringIO import StringIO
from tempfile import mkstemp

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from datetime import datetime

from testtools.matchers import Equals

from lae_automation import server
from lae_automation.signup import DeploymentConfiguration, SubscriptionDetails
from lae_automation.server import api

from .testcase import TestBase
from .matchers import hasLocationHint
from .strategies import (
    nickname, bucket_name, ipv4_address, aws_access_key_id, aws_secret_key,
    furl,
)

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
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('rm -rf /home/customer/LAFS_source', False, {}),
            ('git clone -b 2237-cloud-backend-s4 https://github.com/tahoe-lafs/tahoe-lafs.git LAFS_source', False, {}),
            ('virtualenv venv', False, {}),
            ('venv/bin/pip install --find-links=https://tahoe-lafs.org/deps -e LAFS_source[test]', False, {}),
            ('venv/bin/tahoe create-introducer /home/customer/introducer || echo Assuming that introducer already exists.', False, {}),
            ('venv/bin/tahoe create-node /home/customer/storageserver || echo Assuming that storage server already exists.', False, {})
        ])
        self.SUDOARGS_FIFO = fifo([
            ('apt-get update', False, {}),
            ('apt-get -y install python-dev', False, {}),
            ('apt-get -y install python-pip', False, {}),
            ('apt-get -y install git-core', False, {}),
            ('apt-get -y install libffi-dev', False, {}),
            ('apt-get -y install openssl', False, {}),
            ('apt-get -y install libssl-dev', False, {}),
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
            ('pip install virtualenv', False, {}),
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


class NewTahoeConfigurationTests(TestBase):
    """
    Tests for ``new_tahoe_configuration``.
    """
    @given(
        nickname(), bucket_name(),
        ipv4_address(), ipv4_address(),
        aws_access_key_id(), aws_secret_key(),
        furl(), furl(),
    )
    # Limit the number of iterations of this test - generating RSA
    # keys is slow.
    @settings(max_examples=10)
    def test_generated(
            self,
            nickname, bucket_name,
            publichost, privatehost,
            s3_access_key_id, s3_secret_key,
            log_gatherer_furl, stats_gatherer_furl,
    ):
        """
        New introducer and storage configuration can be created with
        ``new_tahoe_configuration``.
        """
        deploy_config = DeploymentConfiguration(
            products=[{}],
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
            amiimageid=None,
            instancesize=None,

            usertoken=None,
            producttoken=None,

            secretsfile=fdopen(mkstemp()[0], "w"),
            serverinfopath=None,

            ssec2_access_key_id=None,
            ssec2_secret_path=None,

            ssec2admin_keypair_name=None,
            ssec2admin_privkey_path=None,

            monitor_pubkey_path=None,
            monitor_privkey_path=None,

            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        )
        subscription = SubscriptionDetails(
            bucketname=bucket_name,
            oldsecrets=None,
            customer_email=None,
            customer_pgpinfo=None,
            product_id=None,
            customer_id=None,
            subscription_id=None,
        )
        config = server.new_tahoe_configuration(
            deploy_config, subscription, publichost, privatehost,
        )
        # It returns an object which can be round-tripped through the
        # JSON format.
        self.expectThat(config, Equals(loads(dumps(config))))

        # The introducer and storage are both told the same introducer
        # furl.
        self.expectThat(
            config["introducer"]["introducer_furl"],
            Equals(config["storage"]["introducer_furl"]),
        )

        # The introducer furl is contains a location hint of the
        # public host and the hard-coded introducer port we use.
        self.expectThat(
            config["introducer"]["introducer_furl"],
            hasLocationHint(config["storage"]["publichost"], INTRODUCER_PORT),
        )
