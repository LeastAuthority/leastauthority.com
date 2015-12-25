
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from twisted.internet import defer
from twisted.python.failure import Failure
from twisted.web.http_headers import Headers
from twisted.web.client import ResponseDone

from lae_automation.monitor import check_server_by_login, check_servers
from lae_automation import monitor, server, endtoend


class TestServerMonitoring(TestCase):
    PUBLICHOST = '0.0.0.0'
    PRIVHOST = '1.1.1.1'
    MONPRVKEYPATH = 'a_fake_path'
    BUCKETNAME = "bucket"
    SECRETSPATH = "test_secrets"
    STDOUT = StringIO()
    STDERR = StringIO()

    HOSTLIST = [PUBLICHOST, PUBLICHOST]
    SF = endtoend.SecretsFile(filepath=FilePath(SECRETSPATH), secrets={})
    SECRETS_BY_BUCKET = {BUCKETNAME: SF}
    SECRETS_BY_HOST = {PUBLICHOST: SF}

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

        def call_set_host_and_key(publichost, monitor_privkey_path, username=None):
            self.failUnlessEqual(publichost, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server_by_login(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


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

        def call_set_host_and_key(publichost, monitor_privkey_path, username=None):
            self.failUnlessEqual(publichost, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server_by_login(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


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

        def call_set_host_and_key(publichost, monitor_privkey_path, username=None):
            self.failUnlessEqual(publichost, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server_by_login(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


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

        def call_set_host_and_key(publichost, monitor_privkey_path, username=None):
            self.failUnlessEqual(publichost, self.PUBLICHOST)
            self.failUnlessEqual(monitor_privkey_path, self.MONPRVKEYPATH)
            self.failUnlessEqual(username, 'monitor')
        self.patch(monitor, 'set_host_and_key', call_set_host_and_key)

        check_server_by_login(self.PUBLICHOST, self.MONPRVKEYPATH, self.STDOUT, self.STDERR)


    def _test_checkservers(self, by_login, end_to_end):
        def call_check_server_by_login(publichost, monitor_privkey_path, stdout, stderr):
            return (None, by_login)
        self.patch(monitor, 'check_server_by_login', call_check_server_by_login)

        def call_check_server_end_to_end(secretsfp, secrets, stdout, stderr, recreate_test_file=False, timestamp=None):
            return defer.succeed(end_to_end)
        self.patch(endtoend, 'check_server_end_to_end', call_check_server_end_to_end)

        d = check_servers(self.HOSTLIST, self.MONPRVKEYPATH, self.SECRETS_BY_BUCKET, self.SECRETS_BY_HOST,
                          self.STDOUT, self.STDERR, skip_end_to_end=False, recreate_test_file=False)
        d.addCallback(lambda result: self.failUnlessEqual(result, by_login and end_to_end))
        return d


    def test_checkservers_true(self):
        return self._test_checkservers(True, True)

    def test_checkservers_login_false(self):
        return self._test_checkservers(False, True)

    def test_checkservers_endtoend_false(self):
        return self._test_checkservers(True, False)


    def test_compare_servers_to_local_success(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-01-01T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCSTATE = 'running'
        LOCALSTATEDICT = {LOCINSTANCEID : (LOCSTARTTIME, LOCPUBIP, LOCSTATE)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        STATE = 'running'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, STATE)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_unexpectedinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-01-01T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCSTATE = 'running'
        LOCALSTATEDICT = {LOCINSTANCEID : (LOCSTARTTIME, LOCPUBIP, LOCSTATE)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        STATE = 'running'

        INSTANCEID2 = 'i-'+'b'*8
        STARTTIME2 = '2012-01-01T01:00:00.000Z'
        PUBDNSNAME2 = 'ec2-2-2-2-2.c'
        STATE2 = 'running'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, STATE),
                             (STARTTIME2, INSTANCEID2, PUBDNSNAME2, STATE2)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_missingremoteinstance(self):
        LOCINSTANCEID = 'i-'+'a'*8
        LOCSTARTTIME  = '2012-01-01T00:00:00.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCSTATE = 'running'
        LOCINSTANCEID2 = 'i-'+'b'*8
        LOCSTARTTIME2 = '2012-01-01T01:00:00.000Z'
        LOCPUBIP2 = '3.3.3.3'
        LOCSTATE2 = 'running'
        LOCALSTATEDICT = {LOCINSTANCEID : (LOCSTARTTIME, LOCPUBIP, LOCSTATE),
                          LOCINSTANCEID2 : (LOCSTARTTIME2, LOCPUBIP2, LOCSTATE2)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        PUBDNSNAME = 'ec2-0-0-0-0.c'
        STATE = 'running'

        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, PUBDNSNAME, STATE)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


    def test_compare_servers_to_local_mismatchdata(self):
        LOCINSTANCEID = 'i-aaaaaaaa'
        LOCSTARTTIME  = '2012-01-01T01:01:01.000Z'
        LOCPUBIP = '0.0.0.0'
        LOCSTATE = 'running'
        LOCALSTATEDICT = {LOCINSTANCEID : (LOCSTARTTIME, LOCPUBIP, LOCSTATE)}

        INSTANCEID = LOCINSTANCEID
        STARTTIME = LOCSTARTTIME
        DIFFPUBDNSNAME = 'ec2-0-0-0-1.c'
        STATE = 'stopped'
        REMOTEPROPTUPLIST = [(STARTTIME, INSTANCEID, DIFFPUBDNSNAME, STATE)]
        monitor.compare_servers_to_local(REMOTEPROPTUPLIST, LOCALSTATEDICT, self.STDOUT, self.STDERR)


class MockResponse(object):
    def __init__(self, code, phrase, body=""):
        self.code = code
        self.phrase = phrase
        self.body = body

    def deliverBody(self, collector):
        collector.dataReceived(self.body)
        collector.connectionLost(Failure(ResponseDone("EOF")))


def make_mock_agent_class(response):
    class MockAgent(object):
        agent = None
        def __init__(self, *args, **kargs):
            MockAgent.agent = self

        def request(self, method, url, headers, pool):
            self.method = method
            self.url = url
            self.headers = headers
            self.pool = pool
            if isinstance(response, Failure):
                return response
            else:
                return defer.succeed(response)

    return MockAgent


class TestInfrastructureMonitoring(TestCase):
    MOCKURL = "https://0.0.0.0/"

    def _test_check_infrastructure(self, response, response_text):
        stdout = StringIO()
        stderr = StringIO()
        mock_agent_class = make_mock_agent_class(response)
        self.patch(monitor, 'Agent', mock_agent_class)

        d = monitor.check_infrastructure(self.MOCKURL, stdout, stderr)
        def _check(ign):
            agent = mock_agent_class.agent
            self.failUnlessEqual(agent.method, 'GET')
            self.failUnlessEqual(agent.url, self.MOCKURL)
            self.failUnlessIsInstance(agent.headers, Headers)
            self.failUnlessEqual(agent.pool, None)
            stream = stdout if response.code == 200 else stderr
            self.failUnlessEqual(stream.getvalue(),
                                 "Response for %s: %s" % (self.MOCKURL, response_text))
        d.addCallback(_check)
        return d

    def test_check_infrastructure_ok(self):
        return self._test_check_infrastructure(MockResponse(200, "OK"), "200 OK\n")

    def test_check_infrastructure_unexpected(self):
        return self._test_check_infrastructure(MockResponse(204, "No content", ""), "204 No content\n\n")

    def test_check_infrastructure_bad(self):
        return self._test_check_infrastructure(MockResponse(500, "Arrgh", "Oops"), "500 Arrgh\nOops\n")

    def test_check_infrastructure_fail(self):
        d = self._test_check_infrastructure(Failure(Exception("failed")), "")
        d.addCallbacks(lambda ign: self.fail("should not succeed"), lambda f: f.trap(Exception))
        return d

    def test_monitoring_check(self):
        pass
