import sys

from twisted.internet import defer
from foolscap.api import Tub
from foolscap.appserver.client import RunCommand, ClientOptions


class CommandFailed(Exception):
    def __init__(self, rc):
        Exception.__init__(self, "Command failed with exit code %d" % (rc,))
        self.rc = rc


class FlappCommand(object):
    def __init__(self, furlfile):
        self.flappclient_args = ["-f", furlfile, "run-command"]
        options = ClientOptions()
        options.parseOptions(self.flappclient_args)
        self.furl = options.furl
        self.tub = Tub()
        self.rref = None
        self.d = defer.succeed(None)

    def start(self):
        self.d.addCallback(lambda ign: self.tub.startService())
        self.d.addCallback(lambda ign: self.tub.getReference(self.furl))

        done = defer.Deferred()
        def _got_rref(rref):
            self.rref = rref
            done.callback(None)
        def _failed(f):
            done.errback(f)
            return f
        self.d.addCallbacks(_got_rref, _failed)
        return done

    def run(self, content):
        assert isinstance(content, bytes), (`content`, type(content))
        assert self.rref is not None
        options = ClientOptions()
        options.parseOptions(self.flappclient_args)

        def stdio(proto):
            # This value is being sent to the stdin of the flapp.
            proto.dataReceived(content)
            proto.connectionLost("EOF")

        options.subOptions.stdio = stdio
        options.subOptions.stdout = sys.stdout
        options.subOptions.stderr = sys.stderr

        done = defer.Deferred()
        def _go(ign):
            return RunCommand().run(self.rref, options.subOptions)
        def _check_return_code(rc):
            if rc != 0:
                raise CommandFailed(rc)

        self.d.addCallback(_go)
        self.d.addCallback(_check_return_code)
        self.d.addBoth(done.callback)
        return done
