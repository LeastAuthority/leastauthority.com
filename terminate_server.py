#!/usr/bin/python

import os, sys

from twisted.internet import reactor
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath

from lae_automation.terminate import terminate_customer_server


if len(sys.argv) < 3:
    print "Usage: python terminate_server.py EMAIL CUSTOMER_ID SECRETS_PATH"
    print "Happy server terminating!"
    sys.exit(1)

email = sys.argv[1]
customerid = sys.argv[2]
secretsdirfp = FilePath(sys.argv[3])


def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = terminate_customer_server(email, customerid, secretsdirfp)
d.addBoth(cb)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()

