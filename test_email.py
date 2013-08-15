#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure

from lae_util.send_email import send_plain_email, FROM_EMAIL


if len(sys.argv) < 4:
    print "Usage: python test_email.py FROM_ADDRESS TO_EMAIL SUBJECT"
    print "Happy email testing!"
    sys.exit(1)

from_address = sys.argv[1]
to_email = sys.argv[2]
subject = sys.argv[3]


def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = send_plain_email(FROM_EMAIL, to_email, "Hello, this is a test.",
                     {"From": from_address, "Subject": subject})
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()


