#!/usr/bin/python

import os, sys

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_automation.endtoend import check_end_to_end


if __name__ == '__main__':
    if len(sys.argv) < 2 or (len(sys.argv) >= 3 and sys.argv[2] != "--create-test-uri"):
        print "Usage: python check_end_to_end.py SECRETS_PATH [--create-test-uri]"
        print "Happy end-to-end checking!"
        sys.exit(1)

    secretsfile = FilePath(sys.argv[1])
    create_test_uri = (len(sys.argv) >= 3)
    stderr = sys.stderr

    def _err(f):
        print >>stderr, str(f)
        if hasattr(f.value, 'response'):
            print >>stderr, f.value.response
        print >>stderr, "%s: %s" % (f.value.__class__.__name__, f.value)
        return f

    d = defer.succeed(None)
    d.addCallback(lambda ign: check_end_to_end(secretsfile, stderr, create_test_uri))
    d.addErrback(_err)
    d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
    reactor.run()
