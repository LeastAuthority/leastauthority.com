#!/usr/bin/python

import os, sys

from twisted.internet import defer, reactor


class UnbufferedOutputStream:
    def __init__(self, stream):
        self.stream = stream

    def write(self, s):
        self.stream.write(s)
        self.stream.flush()

    def writelines(self, seq):
        for s in seq:
            self.stream.write(s)
        self.stream.flush()

    def flush(self):
        self.stream.flush()

    def isatty(self):
        return self.stream.isatty()

    def close(self):
        self.stream.close()


def main(stdin, stdout, stderr):
    print >>stdout, "Automation script started."
    print >>stderr, "On separate lines: Activation key, Product code, Name, Email, Key info"
    activationkey = stdin.readline().strip()
    productcode = stdin.readline().strip()
    name = stdin.readline().strip()
    email = stdin.readline().strip()
    keyinfo = stdin.readline().strip()

    if keyinfo is None:
        # EOF reached before 5 lines (including blank lines) were input
        raise AssertionError("full_signup.py: some information was not received. Please report this to <info@leastauthority.com>.")

    print >>stderr, "Received all fields, thanks."
    try:
        from lae_automation.signup import signup
        return signup(activationkey, productcode, name, email, keyinfo, stdout, stderr)
    except Exception:
        import traceback
        traceback.print_exc(100, stdout)
        raise

if __name__ == '__main__':
    stdin = sys.stdin
    stdout = UnbufferedOutputStream(sys.stdout)
    stderr = sys.stderr

    # This is to work around the fact that fabric echoes all commands and output to sys.stdout.
    # It does have a way to disable that, but not (easily) to redirect it.
    sys.stdout = sys.stderr

    def _err(f):
        print >>stderr, str(f)
        if hasattr(f.value, 'response'):
            print >>stderr, f.value.response
        print >>stdout, "%s: %s" % (f.value.__class__.__name__, f.value)
        return f

    d = defer.succeed(None)
    d.addCallback(lambda ign: main(stdin, stdout, stderr))
    d.addErrback(_err)
    d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
    reactor.run()
