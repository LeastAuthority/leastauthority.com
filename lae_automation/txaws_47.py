# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Hotfix for https://github.com/twisted/txaws/issues/47
"""

from os import environ

from pem import parse

from twisted.python.filepath import FilePath
from twisted.internet.ssl import Certificate, trustRootFromCertificates
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
        base.WebVerifyingContextFactory = lambda host: BrowserLikePolicyForHTTPS(
            trustRoot=_trust_root_from_environ(environ),
        )


def _trust_root_from_environ(environ):
    try:
        bundle_name = environ[u"CA_CERT_BUNDLE"]
    except KeyError:
        # Without the environment variable, hopefully we can just figure
        # things out ourselves.
        return
    else:
        # If it was set, construct a root that uses all the certs from the
        # identified file.  A bundle should just be one cert after the next.
        bundle_path = FilePath(bundle_name)
        certs = parse(bundle_path.getContent())
        return trustRootFromCertificates(list(
            Certificate.loadPEM(cert.as_bytes())
            for cert
            in certs
        ))
