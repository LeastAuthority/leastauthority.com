
from cStringIO import StringIO
from twisted.trial.unittest import TestCase

from lae_automation.monitor import check_server, check_servers
from lae_automation import monitor
from lae_automation import server


class TestServerMonitoring(TestCase):
    PUBLICHOST = '0.0.0.0'
    PRIVHOST = '1.1.1.1'
    MONPRVKEYPATH = 'a_fake_path'
    STDOUT = StringIO()
    STDERR = StringIO()

    HOSTLIST = [(PUBLICHOST, PRIVHOST), (PUBLICHOST, PRIVHOST)]
    def test_checkserver_failonUID(self):
        PSLINES = """FAILID"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe || true')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_set_host_and_key(public_host, monitor_privkey_path, username=None):
            self.failUnlessEqual(public_host, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_failonargnum(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart storageserver"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe || true')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_set_host_and_key(public_host, monitor_privkey_path, username=None):
            self.failUnlessEqual(public_host, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_failonnodes(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe || true')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_set_host_and_key(public_host, monitor_privkey_path, username=None):
            self.failUnlessEqual(public_host, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_withps_success(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart storageserver"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe || true')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_set_host_and_key(public_host, monitor_privkey_path, username=None):
            self.failUnlessEqual(public_host, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserverS_true(self):
        def call_check_server(public_host, monitor_privkey_path, stdout, stderr):
            return True
        self.patch(monitor, 'check_server', call_check_server)
        result = check_servers(self.HOSTLIST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)
        self.failUnlessEqual(result, True)


    def test_checkserverS_false(self):
        def call_check_server(public_host, monitor_privkey_path, stdout, stderr):
            return False
        self.patch(monitor, 'check_server', call_check_server)
        result = check_servers(self.HOSTLIST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)
        self.failUnlessEqual(result, False)


    def test_compare_servers_to_local_success(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID, LOCPUBIP)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_unexpectedinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID, LOCPUBIP)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'

        INSTANCEID2 = 'i-'+'b'*8
        STARTTIME2 = '2012-00-00T01:00:00.000Z'
        PUBDNSNAME2 = 'ec2-2-2-2-2.c'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME),
                             (STARTTIME2, INSTANCEID2, PUBDNSNAME2)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_missingremoteinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCPUBIP2 = '3.3.3.3'
        LOCINSTANCEID2 = 'i-'+'b'*8
        LOCSTARTTIME2 = '2012-00-00T01:00:00.000Z'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID, LOCPUBIP),
                          LOCPUBIP2 : (LOCSTARTTIME2, LOCINSTANCEID2, LOCPUBIP2)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'

        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_mismatchdata(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID, LOCPUBIP)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        DIFFPUBDNSNAME = 'ec2-0-0-0-1.c'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, DIFFPUBDNSNAME)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)
