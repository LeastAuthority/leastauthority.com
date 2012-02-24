#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_util.send_email import send_plain_email


if len(sys.argv) < 10:
    print "Usage: python test_email.py SMTP_HOST SMTP_USERNAME SMTP_PASSWORD FROM_EMAIL TO_EMAIL SUBJECT SENDER_DOMAIN SMTP_PORT REQUIRE_SSL"
    print "REQUIRE_SSL = 0|1"
    print "Happy email testing!"
    sys.exit(1)

smtphost = sys.argv[1]
smtpusername = sys.argv[2]
smtppassword = sys.argv[3]
fromemail = sys.argv[4]
toemail = sys.argv[5]
subject = sys.argv[6]
senderdomain = sys.argv[7]
smtpport = sys.argv[8]
requiressl = bool(int(sys.argv[9]))


def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = send_plain_email(smtphost, smtpusername, smtppassword, fromemail, toemail, "Hello, this is a test.",
                     {"From": fromemail, "Subject": subject}, senderdomain, smtpport, requiressl)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()


