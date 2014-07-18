#!/usr/bin/python

import sys, os

from twisted.internet import reactor

from lae_automation.monitor import check_infrastructure, monitoring_check


lasterrorspath = '../lasterrors_website.txt'
WEBSITE_URL = "https://leastauthority.com/"

def checker(stdout, stderr):
    return check_infrastructure(WEBSITE_URL, stdout, stderr)

d = monitoring_check(checker, lasterrorspath, sys.stdout, sys.stderr)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
