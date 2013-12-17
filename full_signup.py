#!/usr/bin/env python

import simplejson, os, sys

from twisted.internet import defer, reactor

def main(stdin, stdout, stderr):
    print >>stdout, "Automation script started."
    
    (customer_email,
     customer_pgpinfo,
     customer_id,
     customer_subscription_id,
     customer_plan) = simplejson.loads(stdin.read())

    secretsfile = open(secretsfile_name, 'a')
    logfile = open(logfile_name, 'a')

    print >>stderr, "customer plan is: %s" % (customer_plan,)
    print >>stderr, "customer email is: %s" % (customer_email,)

    #XXX THese secrets have to be moved to website/secrets.  I need to add naming info to each line in the log.
    print >>stderr, "%s " * 5 % (customer_email, customer_pgpinfo, customer_id, 
                                 customer_subscription_id, customer_plan)


    if logfile_name is None:
        # EOF reached before 8 lines (including blank lines) were input
        raise AssertionError("full_signup.py: some information was not received. Please report this to <info@leastauthority.com>.")

    print >>stderr, "Received all fields, thanks."

    try:
        from lae_automation.signup import activate_subscribed_service
        return activate_subscribed_service(customer_email, customer_pgpinfo, 
                                           customer_id, customer_subscription_id, customer_plan)
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
        def _print_except(f):
            f.printTraceback(file=stderr)
            os._exit(2)
        d.addCallbacks(lambda ign: os._exit(0), _print_except)
        reactor.run()
    except Exception:
        import traceback
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        os._exit(1)

