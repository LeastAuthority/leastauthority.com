# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Hotfix for https://github.com/twisted/txaws/issues/47
"""

from twisted.web.client import BrowserLikePolicyForHTTPS

from txaws.client import base

def patch():
    try:
        base.WebVerifyingContextFactory
    except AttributeError:
        # Don't do anything if that thing has gone away.
        pass
    else:
        # Replace that thing with another thing that makes a better thing!
        # See the ticket for details.
        base.WebVerifyingContextFactory = lambda host: BrowserLikePolicyForHTTPS()
