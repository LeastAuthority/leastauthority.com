
from cStringIO import StringIO
from twisted.trial.unittest import TestCase

from lae_automation.monitor import check_server, check_servers
from lae_automation import monitor
from lae_automation import server
import __builtin__

class TestServerMonitoring(TestCase):
    PUBLICHOST = '0.0.0.0'
    PRIVHOST = '1.1.1.1'
    MONPRVKEYPATH = '/a/fake/path'
    STDOUT = StringIO()
    STDERR = StringIO()

    HOSTLIST = [(PUBLICHOST, PRIVHOST), (PUBLICHOST, PRIVHOST)]
    def test_checkserver_failonUID(self):
        PSLINES = """FAILID"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe')
            return PSLINES
        self.patch(monitor, 'run', call_run)
        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_open(filehandle, mode='r'):
            self.failUnlessEqual(filehandle, '/a/fake/path')
        self.patch(__builtin__, 'open', call_open)
        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_failonargnum(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart storageserver"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_open(filehandle, mode='r'):
            self.failUnlessEqual(filehandle, '/a/fake/path')
        self.patch(__builtin__, 'open', call_open)
        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_failonnodes(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_open(filehandle, mode='r'):
            self.failUnlessEqual(filehandle, '/a/fake/path')
        self.patch(__builtin__, 'open', call_open)
        check_server(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def test_checkserver_withps_success(self):
        PSLINES = """UID        PID  PPID  C STIME TTY          TIME CMD\ncustomer   555     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart introducer\ncustomer   564     1  0 05:20 ?        00:00:00 /usr/bin/python /home/customer/LAFS_source/support/bin/tahoe restart storageserver"""
        def call_run(remotecommand):
            self.failUnlessEqual(remotecommand, 'ps -fC tahoe')
            return PSLINES
        self.patch(monitor, 'run', call_run)

        def call_crun(remotecommand, **kwargs):
            self.failUnlessEqual(remotecommand, 'whoami')
            return 'monitor'
        self.patch(server, 'run', call_crun)

        def call_open(filehandle, mode='r'):
            self.failUnlessEqual(filehandle, '/a/fake/path')
        self.patch(__builtin__, 'open', call_open)
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


    def test_comparetolocal_success(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        PRIVDNSNAME = 'ip-1-1-1-1.ec2.internal'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, PRIVDNSNAME)]
        monitor.comparetolocal(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_comparetolocal_unexpectedinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        PRIVDNSNAME = 'ip-1-1-1-1.ec2.internal'

        INSTANCEID2 = 'i-'+'b'*8
        STARTTIME2 = '2012-00-00T01:00:00.000Z'
        PUBDNSNAME2 = 'ec2-2-2-2-2.c'
        PRIVDNSNAME2 = 'ip-3-3-3-3.ec2.internal'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, PRIVDNSNAME),
                             (STARTTIME2, INSTANCEID2, PUBDNSNAME2, PRIVDNSNAME2)]
        monitor.comparetolocal(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_comparetolocal_missingremoteinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'

        LOCPUBIP2 = '3.3.3.3'
        LOCINSTANCEID2 = 'i-'+'b'*8
        LOCSTARTTIME2 = '2012-00-00T01:00:00.000Z'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID),
                          LOCPUBIP2 : (LOCSTARTTIME2, LOCINSTANCEID2)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        PRIVDNSNAME = 'ip-1-1-1-1.ec2.internal'

        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, PRIVDNSNAME)]
        monitor.comparetolocal(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_comparetolocal_mismatchdata(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-00-00T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCALSTATEDICT = {LOCPUBIP : (LOCSTARTTIME, LOCINSTANCEID)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        DIFFPUBDNSNAME = 'ec2-0-0-0-1.c'
        PRIVDNSNAME = 'ip-1-1-1-1.ec2.internal'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, DIFFPUBDNSNAME, PRIVDNSNAME)]
        monitor.comparetolocal(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)
