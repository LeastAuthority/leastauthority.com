#!/usr/bin/python

import os, sys, simplejson, time, base64

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_automation.signup import replace_server
from lae_util.timestamp import format_iso_time
from lae_util.streams import LoggingStream


# FIXME: duplicates code in full_signup.py
if __name__ == '__main__':
    if len(sys.argv) < 5:
        print "Usage: python replace_server.py OLD_SECRETS_PATH AMI_IMAGE_ID INSTANCE_SIZE CUSTOMER_EMAIL"
        print "Happy replacement!"
        sys.exit(1)

    oldsecretspath = sys.argv[1]
    oldsecrets = simplejson.loads(FilePath(oldsecretspath).getContent())

    # TODO: ideally these would be in the secrets file.
    amiimageid = sys.argv[2]
    instancesize = sys.argv[3]
    customer_email = sys.argv[4]

    basefp = FilePath('..')
    seed = base64.b32encode(os.urandom(20)).rstrip('=').lower()
    logfilename = "%s-%s" % (format_iso_time(time.time()).replace(':', ''), seed)

    secretsfile = basefp.child('secrets').child(logfilename).open('a+')
    logfile = basefp.child('signup_logs').child(logfilename).open('a+')
    stdin = sys.stdin
    stdout = LoggingStream(sys.stdout, logfile, '>')
    stderr = LoggingStream(sys.stderr, logfile, '')

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
    d.addCallback(lambda ign: replace_server(oldsecrets, amiimageid, instancesize, customer_email, stdout, stderr,
                                             secretsfile, logfilename))
    d.addErrback(_err)
    d.addBoth(_close)
    d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
    reactor.run()
