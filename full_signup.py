#!/usr/bin/env python

import simplejson, os, sys

from twisted.internet import defer, reactor
from lae_automation.signup import create_log_filepaths
from lae_automation.signup import activate_subscribed_service
from lae_util.streams import LoggingStream
from twisted.python.filepath import FilePath

def main(stdin, stdout, stderr):
    print >>stdout, "Automation script started."
    (customer_email,
     customer_pgpinfo,
     customer_id,
     customer_subscription_id,
     customer_plan) = simplejson.loads(stdin.read())




    stripesecrets_log_fp, SSEC2secrets_log_fp, signup_log_fp = create_log_filepaths(customer_id, 
                                                                                    customer_subscription_id,
                                                                                    customer_plan)

    stripesecrets_log_fp.setContent(simplejson.dumps({
                'customer_email':           customer_email,
                'customer_pgpinfo':         customer_pgpinfo,
                'customer_id':              customer_id,
                'customer_plan':            customer_plan,
                'customer_subscription_id': customer_subscription_id
                } ) )

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    stdout = LoggingStream(signup_logfile, '>')
    stderr = LoggingStream(signup_logfile, '')

    print >> stderr, "%s " * 5 % (customer_email, customer_pgpinfo, customer_id, 
                                          customer_subscription_id, customer_plan)

    try:
        return activate_subscribed_service(customer_email, customer_pgpinfo, customer_id, 
                                           customer_subscription_id, customer_plan, stdout, stderr,
                                           SSEC2_secretsfile, signup_logfile)
    except Exception:
        import traceback
        traceback.print_exc(100, stdout)
        raise

if __name__ == '__main__':
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

    try:
        reactor.run()
    except Exception:
        import traceback
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
        os._exit(1)
