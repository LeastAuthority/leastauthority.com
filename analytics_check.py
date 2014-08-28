#!/usr/bin/python

import sys, os

from twisted.internet import reactor, defer

from lae_automation.monitor import check_infrastructure, monitoring_check


lasterrorspath = '../lasterrors_analytics.txt'
URLS_TO_CHECK = ["https://analytics.leastauthority.com/"]

def checker(stdout, stderr):
    d = defer.succeed(None)
    for url in URLS_TO_CHECK:
        d.addCallback(lambda ign, url=url: check_infrastructure(url, stdout, stderr))
    return d

d = monitoring_check(checker, lasterrorspath, "analytics", "info@leastauthority.com",
                     sys.stdout, sys.stderr)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
