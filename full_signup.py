#!/usr/bin/env python

import simplejson, os, sys

from twisted.internet import defer, reactor

def main(stdin, stdout, stderr):
    print >>stdout, "Automation script started."
    
    (customer_name,
    customer_email,
    customer_pgpinfo,
    customer_id,
    customer_subscription_id,
    customer_plan,
    secretsfile_name,
    logfile_name) = simplejson.loads(stdin.read())

    #We can't pass file object through the foolscap service, so we pass names.
    secretsfile = open(secretsfile_name, 'a')
    logfile = open(logfile_name, 'a')


    print >>stderr, "customer plan is: %s" % (customer_plan,)
    print >>stderr, "customer name is: %s" % (customer_name,)

    print >>stderr, "%s " * 8 % (customer_name, customer_email, customer_pgpinfo, customer_id, 
                                 customer_subscription_id, customer_plan, secretsfile, 
                                 logfile)


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

