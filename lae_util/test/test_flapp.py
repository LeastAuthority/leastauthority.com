
from cStringIO import StringIO
from mock import Mock

from foolscap.api import eventually
from twisted.internet import defer
from twisted.python.filepath import FilePath
from twisted.trial.unittest import TestCase

from lae_util import flapp


class MockTub(object):
    def startService(self):
        pass
    def getReference(self, furl):
        return Mock()

class MockProtocol(object):
    def dataReceived(self, content):
        MockProtocol.content = content
    def connectionLost(self, reason):
        pass

class MockRunCommand(object):
    def run(self, rref, options):
        options.stdio(MockProtocol())
        return defer.succeed(MockRunCommand.rc)


class FlappCommandTests(TestCase):
    def test_flapp(self):
        self.patch(flapp, 'Tub', MockTub)
        self.patch(flapp, 'RunCommand', MockRunCommand)

        FilePath("furlfile").setContent("pb://foo@bar/baz")
        cmd = flapp.FlappCommand("furlfile")
        def raise_exception():
            print >>self.stderr, "CANARY"
            self.ready = True
            raise Exception()
        def callback():
            print >>self.stdout, "DONE"
            self.ready = True
        def fail():
            self.fail("shouldn't get here")
        def _poll_until_ready(ign):
            if self.ready:
                return
            return eventually(_poll_until_ready)
        def reset(rc):
            MockProtocol.content = None
            MockRunCommand.rc = rc
            self.stdout = StringIO()
            self.stderr = StringIO()
            self.ready = False
        reset(0)

        d = cmd.start()

        # check that an exception from when_done doesn't bugger things up
        d.addCallback(lambda ign: cmd.run(u"CONTENT1", self.stdout, self.stderr, raise_exception, fail))
        d.addCallback(_poll_until_ready)
        def _check1(ign):
            self.failUnlessIn("Starting", self.stdout.getvalue())
            self.failIfIn("Command failed", self.stderr.getvalue())
            self.failIfIn("DONE", self.stdout.getvalue())
            self.failUnlessIn("CANARY", self.stderr.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT1")
            reset(0)
        d.addCallback(_check1)

        d.addCallback(lambda ign: cmd.run(u"CONTENT2", self.stdout, self.stderr, callback, fail))
        d.addCallback(_poll_until_ready)
        def _check2(ign):
            self.failUnlessIn("Starting", self.stdout.getvalue())
            self.failIfIn("Command failed", self.stderr.getvalue())
            self.failUnlessIn("DONE", self.stdout.getvalue())
            self.failIfIn("CANARY", self.stderr.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT2")
            reset(1)  # make the next command fail
        d.addCallback(_check2)

        # check that an exception from when_failed doesn't bugger things up
        d.addCallback(lambda ign: cmd.run(u"CONTENT3", self.stdout, self.stderr, fail, raise_exception))
        d.addCallback(_poll_until_ready)
        def _check3(ign):
            self.failUnlessIn("Starting", self.stdout.getvalue())
            self.failUnlessIn("Command failed with exit code 1.", self.stdout.getvalue())
            self.failIfIn("DONE", self.stdout.getvalue())
            self.failUnlessIn("CANARY", self.stderr.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT3")
            reset(1)
        d.addCallback(_check3)

        d.addCallback(lambda ign: cmd.run(u"CONTENT4", self.stdout, self.stderr, fail, callback))
        d.addCallback(_poll_until_ready)
        def _check4(ign):
            self.failUnlessIn("Starting", self.stdout.getvalue())
            self.failUnlessIn("Command failed with exit code 1.", self.stdout.getvalue())
            self.failUnlessIn("DONE", self.stdout.getvalue())
            self.failIfIn("CANARY", self.stderr.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT4")
            reset(0)
        d.addCallback(_check4)
        return d
