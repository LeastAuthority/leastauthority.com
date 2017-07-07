from twisted.internet import defer
from foolscap.api import Tub
from foolscap.appserver.client import RunCommand, ClientOptions


class CommandFailed(Exception):
    def __init__(self, rc):
        Exception.__init__(self, "Command failed with exit code %d" % (rc,))
        self.rc = rc


class FlappCommand(object):
    def __init__(self, furl):
        self.flappclient_args = ["--furl", furl, "run-command"]
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

    def run(self, content, log):
        assert isinstance(content, bytes), (`content`, type(content))
        assert self.rref is not None
        options = ClientOptions()
        options.parseOptions(self.flappclient_args)

        def stdio(proto):
            # This value is being sent to the stdin of the flapp.
            proto.dataReceived(content)
            proto.connectionLost("EOF")

        options.subOptions.stdio = stdio

        # These are not used.
        options.subOptions.stdout = None
        options.subOptions.stderr = None

        print >>log, "Starting command."
        self.d = RunCommand().run(self.rref, options.subOptions)
        def _log_return_code(rc):
            print >>log, "Command completed with exit code %r" % (rc,)
        def _log_failure(f):
            print >>log, "Command failed with %r" % (f,)

        self.d.addCallbacks(_log_return_code, _log_failure)
        return self.d
