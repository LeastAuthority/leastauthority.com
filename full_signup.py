#!/usr/bin/python

import time, os, sys, base64

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_util.streams import LoggingTeeStream
from lae_util.timestamp import format_iso_time


def main(stdin, stdout, stderr):#, seed, secretsfile, logfilename):
    print >>stdout, "Automation script started."
    print >>stderr, "On separate lines: Activation key, Product code, Name, Email, Key info"
    customer_name = stdin.readline().strip()
    customer_email = stdin.readline().strip()
    customer_pgpinfo = stdin.readline().strip()
    customer_id = stdin.readline().strip()
    customer_subscription_id = stdin.readline().strip()
    customer_plan = stdin.readline().strip()
    secretsfile = stdin.readline().strip()
    logfile = stdin.readline().strip()

    if logfile is None:
        # EOF reached before 8 lines (including blank lines) were input
        raise AssertionError("full_signup.py: some information was not received. Please report this to <info@leastauthority.com>.")

    print >>stderr, "Received all fields, thanks."
    try:
        from lae_automation.signup import activate_subscribed_service
        return activate_subscribed_service(customer_name, customer_email, customer_pgpinfo, 
                                           customer_subscription_id, customer_plan, stdout, stderr, 
                                           secretsfile, logfile)
    except Exception:
        import traceback
        traceback.print_exc(100, stdout)
        raise

if __name__ == '__main__':
    try:
        defer.setDebugging(True)
        basefp = FilePath('..')
        seed = base64.b32encode(os.urandom(20)).rstrip('=').lower()
        logfilename = "%s-%s" % (format_iso_time(time.time()).replace(':', ''), seed)

        secretsfile = basefp.child('secrets').child(logfilename).open('a+')
        logfile = basefp.child('signup_logs').child(logfilename).open('a+')
        stdin = sys.stdin
        stdout = LoggingTeeStream(sys.stdout, logfile, '>')
        stderr = LoggingTeeStream(sys.stderr, logfile, '')

        # This is to work around the fact that fabric echoes all commands and output to sys.stdout.
        # It does have a way to disable that, but not (easily) to redirect it.
        sys.stdout = stderr

        def _close(res):
            stdout.flush()
            stderr.flush()
            secretsfile.close()
            logfile.close()
            return res
        def _err(f):
            print >>stderr, str(f)
            if hasattr(f.value, 'response'):
                print >>stderr, f.value.response
            print >>stdout, "%s: %s" % (f.value.__class__.__name__, f.value)
            return f

        d = defer.succeed(None)
        d.addCallback(lambda ign: main(stdin, stdout, stderr, seed, secretsfile, logfilename))
        d.addErrback(_err)
        d.addBoth(_close)
        d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
        reactor.run()
    except Exception:
        import traceback
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        os._exit(1)

