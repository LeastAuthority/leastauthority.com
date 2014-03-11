#!/usr/bin/env python

import simplejson, sys

from twisted.internet import defer, reactor
from lae_automation.signup import create_log_filepaths
from lae_automation.signup import activate_subscribed_service
from lae_util.streams import LoggingStream
from lae_util.servers import append_record
from lae_util.fileutil import make_dirs
from twisted.python.filepath import FilePath

def main(stdin, flapp_stdout, flapp_stderr):
    append_record(flapp_stdout, "Automation script started.")
    (customer_email,
     customer_pgpinfo,
     customer_id,
     plan_id,
     subscription_id) = simplejson.loads(stdin.read())

    abslogdir_fp, stripesecrets_log_fp, SSEC2secrets_log_fp, signup_log_fp = \
        create_log_filepaths(plan_id,
                             customer_id,
                             subscription_id)
    append_record(flapp_stdout, "Writing logs to %r." % (abslogdir_fp.path,))

    stripesecrets_log_fp.setContent(simplejson.dumps({
                'customer_email':                customer_email,
                'customer_pgpinfo':              customer_pgpinfo,
                'customer_id':                   customer_id,
                'plan_id':                       plan_id,
                'subscription_id':               subscription_id
                } ) )

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')

    def errhandler(err):
        fh = flapp_stderr.open('a+')
        fh.write(repr(err))
        fh.close()
        return err

    d = defer.succeed(None)
    d.addCallback(lambda ign: activate_subscribed_service(customer_email, customer_pgpinfo,
                                                          customer_id, subscription_id, plan_id,
                                                          signup_stdout, signup_stderr,
                                                          SSEC2_secretsfile, signup_log_fp.path))
    d.addErrback(errhandler)
    d.addBoth(lambda ign: signup_logfile.close())

if __name__ == '__main__':

    defer.setDebugging(True)
    stdin = sys.stdin
    logDir = FilePath('../secrets/flappserver_logs')
    if not logDir.isdir():
        make_dirs(logDir.path)
    flapp_stdout = logDir.child('stdout')
    flapp_stderr = logDir.child('stderr')

    d = defer.succeed(None)
    d.addCallback(lambda ign: main(stdin, flapp_stdout, flapp_stderr))
    def _print_except(f):
        fh = flapp_stderr.open('a+')
        f.print_stack(file=fh)
        fh.close()

    d.addErrback(_print_except)
    d.addCallbacks(lambda ign: sys.exit(0), lambda ign: sys.exit(8))
    try:
        reactor.run()
    except Exception:
        import traceback
        fh = flapp_stderr.open('a+')
        traceback.print_exc(file=fh)
        fh.close()
        sys.exit(7)
