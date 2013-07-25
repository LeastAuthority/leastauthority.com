#!/usr/bin/python

import os, sys

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath

from lae_automation.server import read_secrets_file
from lae_automation.endtoend import check_server_end_to_end


if __name__ == '__main__':
    if len(sys.argv) < 2 or (len(sys.argv) >= 3 and sys.argv[2] != "--recreate"):
        print "Usage: python check_end_to_end.py SECRETS_PATH [--recreate]"
        print "Options:"
        print "  --recreate    creates a new test file even if one already existed."
        print
        print "Happy end-to-end checking!"
        sys.exit(1)

    secretsfp = FilePath(sys.argv[1])
    secrets = read_secrets_file(secretsfp)
    recreate_test_file = (len(sys.argv) >= 3)
    stdout = sys.stdout
    stderr = sys.stderr

    def _err(f):
        print >>stderr, str(f)
        if hasattr(f.value, 'response'):
            print >>stderr, f.value.response
        print >>stderr, "%s: %s" % (f.value.__class__.__name__, f.value)
        return f

    d = defer.succeed(None)
    d.addCallback(lambda ign: check_server_end_to_end(secretsfp, secrets, stdout, stderr, recreate_test_file))
    d.addErrback(_err)
    d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
    reactor.run()
