#!/usr/bin/env python

import time, os, sys, base64

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_util.streams import LoggingTeeStream
from lae_util.timestamp import format_iso_time


def main(stdin, stdout, stderr):
    print >>stdout, "Automation script started."
    print >>stderr, "On separate lines: Name, email, pgpinfo, stripe customer id, stripe subscription id, plan name, secretsfile, logfile"
    customer_name = stdin.readline().strip()
    customer_email = stdin.readline().strip()
    customer_pgpinfo = stdin.readline().strip()
    customer_id = stdin.readline().strip()
    customer_subscription_id = stdin.readline().strip()
    customer_plan = stdin.readline().strip()
    secretsfile_name = stdin.readline().strip()
    logfile_name = stdin.readline().strip()


    print >>stderr, "customer plan is: %s" % (customer_plan,)
    print >>stderr, "customer name is: %s" % (customer_name,)

    print >>stderr, "%s " * 8 % (customer_name, customer_email, customer_pgpinfo, customer_id, 
                                 customer_subscription_id, customer_plan, secretsfile_name, 
                                 logfile_name)


    if logfile_name is None:
        # EOF reached before 8 lines (including blank lines) were input
        raise AssertionError("full_signup.py: some information was not received. Please report this to <info@leastauthority.com>.")

    print >>stderr, "Received all fields, thanks."

    try:
        from lae_automation.signup import activate_subscribed_service
        return activate_subscribed_service(customer_name, customer_email, customer_pgpinfo, 
                                           customer_id, customer_subscription_id, customer_plan, 
                                           stdout, stderr, secretsfile, logfile)
    except Exception:
        import traceback
        traceback.print_exc(100, stdout)
        raise

if __name__ == '__main__':
    try:
        defer.setDebugging(True)
        stdin = sys.stdin
        stdout = sys.stdout
        stderr = sys.stderr

        d = defer.succeed(None)
        d.addCallback(lambda ign: main(stdin, stdout, stderr))
        d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
        reactor.run()
    except Exception:
        import traceback
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        os._exit(1)

