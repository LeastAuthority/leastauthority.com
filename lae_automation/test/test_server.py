from cStringIO import StringIO
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

class TestServerModule(TestCase):
    def setUp(self):
        def call_api_run(argstring, pty, **kwargs):
            self.failUnlessEqual(self.RUNARGS_FIFO.pop(), (argstring, pty, kwargs))
            if argstring == 'whoami':
                return self.WHOAMI_FIFO.pop()
        self.patch(api, 'run', call_api_run)

        def call_api_sudo(argstring, pty=False, **kwargs):
            self.failUnlessEqual(self.SUDOARGS_FIFO.pop(), (argstring, pty, kwargs))
        self.patch(api, 'sudo', call_api_sudo)

        def call_api_reboot(seconds, *args, **kwargs):
            self.failUnlessEqual(seconds, 60)
        self.patch(api, 'reboot', call_api_reboot)

        def call_write(value, remote_path, use_sudo=False, mode=None):
            self.failUnlessEqual(self.WRITEARGS_FIFO.pop(), (value, remote_path, use_sudo, mode))
            return [remote_path]
        self.patch(server, 'write', call_write)

        self.CONFIGFILEPATH = 'init_test_config.json'
        FilePath(self.CONFIGFILEPATH).setContent(CONFIGFILEJSON)

    def tearDown(self):
        FilePath(self.CONFIGFILEPATH).remove()

    def _check_all_done(self):
        self.failUnlessEqual(self.WHOAMI_FIFO, [])
        self.failUnlessEqual(self.RUNARGS_FIFO, [])
        self.failUnlessEqual(self.SUDOARGS_FIFO, [])
        self.failUnlessEqual(self.WRITEARGS_FIFO, [])

    def tearDown(self):
        self._check_all_done()

    def test_initialize_statmover_source(self):
        MHOSTNAME = '0.0.0.0'
        MINSTANCEID = 'i-MOCKEC2INSTANCEID'
        ADMINPRIVKEYPATH = 'mockEC2adminkeys.pem'
        MONITORPRIVKEYPATH = 'mockEC2monitorkeys.pem'
        MPATHTOSTATMOVER = '../'+server.INSTALL_STATMOVER_PACKAGE

        def call_subprocess_check_output(arglist):
            self.failUnlessEqual(arglist[0], "scp")
            self.failUnlessEqual(arglist[1], "-i")
            self.failUnlessEqual(arglist[2], MONITORPRIVKEYPATH)
            self.failUnlessEqual(arglist[3], MPATHTOSTATMOVER)
            self.failUnlessEqual(arglist[4], "monitor@"+MHOSTNAME+":")
        self.patch(server.subprocess, "check_output", call_subprocess_check_output)

        self.WHOAMI_FIFO = fifo(['ubuntu', 'monitor', 'ubuntu', 'monitor'])
        self.RUNARGS_FIFO = fifo([
            ('whoami', False, {}),
            ('whoami', False, {}),
            ('tar -xzvf %s' % (server.INSTALL_STATMOVER_PACKAGE,), False, {}),
            ('mv /home/monitor/statmover/config /home/monitor/.saturnaliaclient', False, {}),
            ('whoami', False, {}),
            ('whoami', False, {}),
            (server.SETUPMETRICTEMPLATE % ('storageserver/rss', ' '.join([MINSTANCEID, 'SSEC2s'])), False, {}),
            ("mkdir -p /home/monitor/statmover/emissionlogs", False, {}),
            ("chmod u+x generatevalues.py", False, {}),
            ("chmod u+x /home/monitor/emissionscript.sh", False, {}),
            ("crontab /home/monitor/ctab", False, {})
        ])
        self.SUDOARGS_FIFO = fifo([
            ('rm -rf statmover* /home/monitor/.satur* /home/monitor/ctab /home/monitor/emissionscript.sh', False, {}),
            ('python ./setup.py install', False, {})
        ])
        self.WRITEARGS_FIFO = fifo([
            (server.GENERATESCRIPT, '/home/monitor/statmover/generatevalues.py', False, None),
            (server.EMITCONFIG_TEMPLATE % ('storageserver/rss',MINSTANCEID+'//SSEC2s'), '/home/monitor/statmover/eventemissions_config.json', False, None),
            (server.CRONEMISSIONSCRIPT, '/home/monitor/emissionscript.sh', False, None),
            ('* * * * * /home/monitor/emissionscript.sh\n', '/home/monitor/ctab', False, None)
        ])
        server.initialize_statmover_source(MHOSTNAME, MONITORPRIVKEYPATH, ADMINPRIVKEYPATH, "storageserver/rss", [MINSTANCEID, 'SSEC2s'])

    def test_install_server(self):
        self.WHOAMI_FIFO = fifo(['ubuntu', 'monitor', 'customer'])
        self.RUNARGS_FIFO = fifo([
            ('whoami', False, {}),
            ('wget https://tahoe-lafs.org/source/tahoe-lafs/deps/tahoe-lafs-dep-sdists/txAWS-0.2.1.post4.tar.gz', False, {}),
            ('tar -xzvf txAWS-0.2.1.post4.tar.gz', False, {}),
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
            ('apt-get dist-upgrade -y', False, {}),
            ('dpkg -P consolekit', False, {}),
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
            ('chmod 700 /home/monitor/.ssh/', False, {})
        ])
        self.WRITEARGS_FIFO = fifo([('THIS IS A MOCK PUBLIC KEY', '/home/monitor/.ssh/authorized_keys', True, None)])

        MHOSTNAME = '0.0.0.0'
        ADMINPRIVKEYPATH = 'mockEC2adminkeys.pem'
        MONITORPUBKEY = 'THIS IS A MOCK PUBLIC KEY'
        MONITORPRIVKEYPATH = 'mockEC2monitorkeys.pem'
        STDOUT = StringIO()
        STDERR = StringIO()

        server.install_server(MHOSTNAME, ADMINPRIVKEYPATH, MONITORPUBKEY, MONITORPRIVKEYPATH, STDOUT, STDERR)

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
                ('chmod u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (acct_name,), False, {}),
                ('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(acct_name,)), False, {}),
                ('chmod 400 /home/%s/.ssh/authorized_keys' % acct_name, False, {}),
                ('chmod 700 /home/%s/.ssh/' % acct_name, False, {})]
            if pubkey is None:
                SUDOARGS.insert(4, ('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys', False, {}))
            self.SUDOARGS_FIFO = fifo(SUDOARGS)
            if pubkey is None:
                self.WRITEARGS_FIFO = fifo([])
            else:
                self.WRITEARGS_FIFO = fifo([(pubkey, '/home/%s/.ssh/authorized_keys' % acct_name, True, None)])

            server.create_account(acct_name, pubkey, STDOUT, STDERR)

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
        USERTOKEN = 'TESTUSERTOKEN'+'A'*385
        PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
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
                                      'access_key_id': ACCESSKEYID,
                                      'bucket_name': BUCKETNAME,
                                      'incident_gatherer_furl': "MOCK_incident_gatherer_furl",
                                      'stats_gatherer_furl': "MOCK_stats_gatherer_furl"}
        self.WHOAMI_FIFO = []
        self.RUNARGS_FIFO = fifo([
                ('rm -f /home/customer/introducer/introducer.furl', False, {}),
                ('LAFS_source/bin/tahoe restart introducer && sleep 5', False, {}),
                ('cat /home/customer/introducer/introducer.furl', False, {}),
                ('chmod u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.', False, {}),
                ('LAFS_source/bin/tahoe restart storageserver && sleep 5', False, {}),
                ('ps -fC tahoe', False, {}),
                ('netstat -atW', False, {}),
                ('crontab /home/customer/ctab', False, {}),
                ('cat /home/customer/introducer/private/node.pem', False, {}),
                ('cat /home/customer/introducer/my_nodeid', False, {}),
                ('cat /home/customer/storageserver/private/node.pem', False, {}),
                ('cat /home/customer/storageserver/my_nodeid', False, {})
                ])
        self.SUDOARGS_FIFO = []
        self.WRITEARGS_FIFO = fifo([
                (INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port', False, None),
                (SERVER_PORT + '\n', '/home/customer/storageserver/client.port', False, None),
                (test_tahoe_cfg, '/home/customer/storageserver/tahoe.cfg', False, None),
                (SECRETACCESSKEY, '/home/customer/storageserver/private/s3secret', False, 0440),
                (USERTOKEN, '/home/customer/storageserver/private/s3usertoken', False, 0440),
                (PRODUCTTOKEN, '/home/customer/storageserver/private/s3producttoken', False, 0440),
                (RESTART_SCRIPT, '/home/customer/restart.sh', False, 0750),
                ('@reboot /home/customer/restart.sh\n', '/home/customer/ctab', False, None)
                ])
        server.bounce_server(MHOSTNAME, ADMINPRIVKEYPATH, MPRIVHOST, ACCESSKEYID, \
                             SECRETACCESSKEY, USERTOKEN, PRODUCTTOKEN, BUCKETNAME, None, \
                             STDOUT, STDERR, MSECRETSFILE, self.CONFIGFILEPATH)
        self._check_all_done()

