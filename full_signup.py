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
    parameters_json = stdin.read()
    (customer_email,
     customer_pgpinfo,
     customer_id,
     plan_id,
     subscription_id) = simplejson.loads(parameters_json)

    (abslogdir_fp,
     stripe_secretsfp,
     ssec2_secretsfp,
     signup_logfp) = create_log_filepaths(plan_id, customer_id, subscription_id)

    append_record(flapp_stdout, "Writing logs to %r." % (abslogdir_fp.path,))

    stripe_secretsfp.setContent(parameters_json)

    signup_logfile = signup_logfp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')
    sys.stdout = signup_stderr

    def errhandler(err):
        fh = flapp_stderr.open('a+')
        fh.write(repr(err))
        fh.close()
        return err

    d = defer.succeed(None)
    d.addCallback(lambda ign: activate_subscribed_service(customer_email, customer_pgpinfo,
                                                          customer_id, subscription_id,
                                                          plan_id,
                                                          signup_stdout, signup_stderr,
                                                          ssec2_secretsfp, signup_logfp.path)
                  )
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
