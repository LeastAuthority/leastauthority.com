#!/usr/bin/env python

import simplejson, os, sys

from twisted.internet import defer, reactor
from lae_automation.signup import create_log_filepaths
from lae_automation.signup import activate_subscribed_service
from lae_util.streams import LoggingStream
from lae_util.servers import append_record
from twisted.python.filepath import FilePath

def main(stdin, flapp_stdout, flapp_stderr):
    append_record(flapp_stdout, "Automation script started.")
    (customer_email,
     customer_pgpinfo,
     customer_id,
     customer_subscription_plan_id,
     customer_subscription_id) = simplejson.loads(stdin.read())

    stripesecrets_log_fp, SSEC2secrets_log_fp, signup_log_fp = create_log_filepaths(customer_subscription_plan_id,
                                                                                    customer_id, 
                                                                                    customer_subscription_id)

    stripesecrets_log_fp.setContent(simplejson.dumps({
                'customer_email':                customer_email,
                'customer_pgpinfo':              customer_pgpinfo,
                'customer_id':                   customer_id,
                'customer_subscription_plan_id': customer_subscription_plan_id,
                'customer_subscription_id':      customer_subscription_id
                } ) )

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')

    try:
        return activate_subscribed_service(customer_email, customer_pgpinfo, customer_id, 
                                           customer_subscription_id, customer_subscription_plan_id,  
                                           signup_stdout, signup_stderr, SSEC2_secretsfile, 
                                           signup_logfile)
    except Exception:
        import traceback
        fh = flapp_stderr.open('a+')
        traceback.print_exc(100, file=fh)
        fh.close()
        raise

if __name__ == '__main__':

    defer.setDebugging(True)
    stdin = sys.stdin
    flapp_stdout = FilePath('../secrets/flappserver_logs/stdout')
    flapp_stderr = FilePath('../secrets/flappserver_logs/stderr')
    
    d = defer.succeed(None)
    d.addCallback(lambda ign: main(stdin, flapp_stdout, flapp_stderr))
    def _print_except(f):
        fh = flapp_stderr.open('a+')
        f.print_stack(file=fh)
        fh.close()

    d.addErrback(_print_except)
    d.addCallbacks(lambda ign: os._exit(0), os._exit(9))
    FilePath('/home/arc/tester').setContent('5')
    try:
        reactor.run()
    except Exception:
        FilePath('/home/arc/tester').setContent('6')
        import traceback
        fh = flapp_stderr.open('a+')
        traceback.print_exc(file=fh)
        fh.close()
        os._exit(7)
