
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
        MockRunCommand.ready = True
        return defer.succeed(MockRunCommand.rc)


class FlappCommandTests(TestCase):
    def test_flapp(self):
        self.patch(flapp, 'Tub', MockTub)
        self.patch(flapp, 'RunCommand', MockRunCommand)

        FilePath("furlfile").setContent("pb://foo@bar/baz")
        cmd = flapp.FlappCommand("furlfile")
        def _poll_until_ready(ign):
            if MockRunCommand.ready:
                return
            return eventually(_poll_until_ready)
        def reset(rc):
            self.log = StringIO()
            MockProtocol.content = None
            MockRunCommand.rc = rc
            MockRunCommand.ready = False
        reset(0)

        d = cmd.start()

        # check the success case
        d.addCallback(lambda ign: cmd.run("CONTENT1", self.log))
        d.addCallback(_poll_until_ready)
        def _check1(ign):
            self.failUnlessIn("Starting", self.log.getvalue())
            self.failUnlessIn("Command completed with exit code 0\n", self.log.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT1")
            reset(1)  # make the next command fail
        d.addCallback(_check1)

        # check the failure case
        d.addCallback(lambda ign: cmd.run("CONTENT2", self.log))
        d.addCallback(_poll_until_ready)
        def _check2(ign):
            self.failUnlessIn("Starting", self.log.getvalue())
            self.failUnlessIn("Command completed with exit code 1\n", self.log.getvalue())
            self.failUnlessEqual(MockProtocol.content, "CONTENT2")
        d.addCallback(_check2)
        return d
