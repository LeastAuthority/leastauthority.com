
from twisted.internet import defer
from foolscap.api import Tub
from foolscap.appserver.client import RunCommand, ClientOptions


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
        self.d.addCallbacks(_got_rref, done.errback)
        return done

    def run(self, content, stdout, stderr, when_done, when_failed):
        assert isinstance(content, bytes), (`content`, type(content))
        assert self.rref is not None
        options = ClientOptions()
        options.stdout = stdout
        options.stderr = stderr
        print >>stderr, 'self.flappclient_args: %s' % self.flappclient_args
        options.parseOptions(self.flappclient_args)

        def stdio(proto):
            print >>stderr, '\n***\ncontent: %s' % content
            #This value is being sent to the stdin of the flapp, and
            #must be coerced to a bytestring.
            proto.dataReceived(content.encode('utf-8'))
            proto.connectionLost("EOF")

        options.subOptions.stdio = stdio
        options.subOptions.stdout = stdout
        options.subOptions.stderr = stderr

        def _go(ign):
            print >>stdout, "Starting..."
            return RunCommand().run(self.rref, options.subOptions)
        def _done(rc):
            if rc == 0:
                when_done()
            else:
                print >>stdout, "Command failed with exit code %r." % (rc,)
                when_failed()
        def _error(f):
            print >>stdout, str(f)
            when_failed()
        def _recover(f):
            try:
                print >>stderr, str(f)
            except Exception:
                print >>stderr, "something weird"

        self.d.addCallback(_go)
        self.d.addCallbacks(_done, _error)
        self.d.addErrback(_recover)
