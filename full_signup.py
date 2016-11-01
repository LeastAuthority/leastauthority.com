#!/usr/bin/env python

if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    from full_signup import main
    react(main, argv[1:])

import simplejson, sys

from twisted.python.log import startLogging
from twisted.python.usage import UsageError, Options
from twisted.python.filepath import FilePath
from twisted.internet import defer

from lae_automation.signup import create_log_filepaths
from lae_automation.signup import activate_subscribed_service
from lae_util.streams import LoggingStream
from lae_util.servers import append_record
from lae_util.fileutil import make_dirs

def activate(secrets_dir, stdin, flapp_stdout, flapp_stderr):
    append_record(flapp_stdout, "Automation script started.")
    parameters_json = stdin.read()
    (customer_email,
     customer_pgpinfo,
     customer_id,
     plan_id,
     subscription_id) = simplejson.loads(parameters_json)

    (abslogdir_fp,
    stripesecrets_log_fp,
    SSEC2secrets_log_fp,
    signup_log_fp) = create_log_filepaths(
        secrets_dir, plan_id, customer_id, subscription_id,
    )

    append_record(flapp_stdout, "Writing logs to %r." % (abslogdir_fp.path,))

    stripesecrets_log_fp.setContent(parameters_json)

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')
    sys.stdout = signup_stderr

    def errhandler(err):
        fh = flapp_stderr.open('a+')
        err.printTraceback(fh)
        fh.close()
        return err

    d = defer.succeed(None)
    d.addCallback(lambda ign: activate_subscribed_service(customer_email, customer_pgpinfo,
                                                          customer_id, subscription_id,
                                                          plan_id,
                                                          signup_stdout, signup_stderr,
                                                          SSEC2_secretsfile, signup_log_fp.path)
                  )
    d.addErrback(errhandler)
    d.addBoth(lambda ign: signup_logfile.close())
    return d


class SignupOptions(Options):
    optParameters = [
        ("log-directory", None, None, "Path to a directory to which to write signup logs.", FilePath),
        ("secrets-directory", None, None, "Path to a directory to which to write subscription secrets.", FilePath),
    ]


def main(reactor, *argv):
    o = SignupOptions()
    try:
        o.parseOptions(argv)
    except UsageError as e:
        raise SystemExit(str(e))

    defer.setDebugging(True)

    log_dir = o["log-directory"]
    secrets_dir = o["secrets-directory"]

    for d in [log_dir, secrets_dir]:
        if not log_dir.isdir():
            make_dirs(log_dir.path)

    flapp_stdout = log_dir.child('stdout')
    flapp_stderr = log_dir.child('stderr')

    startLogging(flapp_stdout, setStdout=False)

    return activate(secrets_dir, sys.stdin, flapp_stdout, flapp_stderr)
