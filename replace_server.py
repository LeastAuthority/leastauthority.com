#!/usr/bin/python

import os, sys, simplejson

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_automation.signup import create_log_filepaths, replace_server
from lae_util.streams import LoggingStream


# FIXME: duplicates code in full_signup.py
if __name__ == '__main__':
    if len(sys.argv) < 5:
        print "Usage: python replace_server.py STRIPE_SECRETS_PATH SSEC2_SECRETS_PATH AMI_IMAGE_ID INSTANCE_SIZE CUSTOMER_EMAIL"
        print "Happy replacement!"
        sys.exit(1)

    # Secrets necessary for correct logging
    stripe_secrets_path = sys.argv[1]
    stripe_secrets = simplejson.loads(FilePath(stripe_secrets_path).getContent())
    stripe_plan_id = stripe_secrets[3]
    stripe_customer_id = stripe_secrets[2]
    stripe_subscription_id = stripe_secrets[4]

    (abslogdir_fp,
    stripesecrets_log_fp,
    SSEC2secrets_log_fp,
    signup_log_fp) = create_log_filepaths(stripe_plan_id, stripe_customer_id, stripe_subscription_id)

    # The stripe secrets are unchanged.
    stripesecrets_log_fp.setContent(simplejson.dumps(stripe_secrets))

    SSEC2_secretsfile = SSEC2secrets_log_fp.open('a+')
    signup_logfile = signup_log_fp.open('a+')
    signup_stdout = LoggingStream(signup_logfile, '>')
    signup_stderr = LoggingStream(signup_logfile, '')
    # This is to work around the fact that fabric echoes all commands and output to sys.stdout.
    # It does have a way to disable that, but not (easily) to redirect it.
    sys.stdout = signup_stderr

    # Secrets necessary to provision SSEC2
    old_ssec2_secrets_path = sys.argv[2]
    old_ssec2_secrets = simplejson.loads(FilePath(old_ssec2_secrets_path).getContent())

    # TODO: ideally these would be in the secrets file.
    amiimageid = sys.argv[3]
    instancesize = sys.argv[4]
    customer_email = sys.argv[5]


    def _close(res):
        sys.stdout.flush()
        sys.stderr.flush()
        signup_logfile.close()
        return res
    def _err(f):
        print >>sys.stderr, str(f)
        if hasattr(f.value, 'response'):
            print >>sys.stderr, f.value.response
        print >>sys.stdout, "%s: %s" % (f.value.__class__.__name__, f.value)
        return f

    d = defer.succeed(None)
    d.addCallback(lambda ign: replace_server(old_ssec2_secrets, amiimageid, instancesize, customer_email, signup_stdout, signup_stderr,
                                             SSEC2_secretsfile, signup_logfile))
    d.addErrback(_err)
    d.addBoth(_close)
    d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
    reactor.run()
