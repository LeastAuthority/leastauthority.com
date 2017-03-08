# before importing Twisted
import mimetypes
mimetypes.add_type("text/plain", ".rst")

if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    from lae_site.main import main

    react(main, argv[1:])

import sys
import logging

from twisted.python.log import startLogging
from twisted.internet.endpoints import serverFromString
from twisted.application.internet import StreamServerEndpointService
from twisted.application.service import MultiService
from twisted.internet.defer import Deferred
from twisted.python.usage import UsageError, Options
from twisted.python.filepath import FilePath

from lae_site.handlers import make_site, make_redirector_site
from lae_site.handlers.submit_subscription import start

root_log = logging.getLogger(__name__)

class SiteOptions(Options):
    optFlags = [
        # TODO:
        # Make this HTTP-only.
        # Terminate TLS externally.
        # On K8S on AWS, consider using
        # http://kubernetes.io/docs/user-guide/services/#ssl-support-on-aws
    ]

    optParameters = [
        ("signup-furl-path", None, None, "A path to a file containing the signup furl.", FilePath),
        ("interest-path", None, None, "A path to a file to which contact information of people interested in products will be appended.", FilePath),
        ("stripe-secret-api-key-path", None, None, "A path to a file containing a Stripe API key.", FilePath),
        ("stripe-publishable-api-key-path", None, None, "A path to a file containing a publishable Stripe API key.", FilePath),
        ("subscriptions-path", None, None, "A path to a file to which new subscription details will be appended.", FilePath),
        ("service-confirmed-path", None, None, "A path to a file to which confirmed-service subscription details will be appended.", FilePath),
        ("site-logs-path", None, None, "A path to a file to which HTTP logs for the site will be written.", FilePath),

        ("redirect-to-port", None, None, "A TCP port number to which to redirect for the TLS site.", int),
    ]
    def __init__(self, reactor):
        Options.__init__(self)
        self.reactor = reactor
        self["secure-ports"] = []
        self["insecure-ports"] = []

    def _parse_endpoint(self, description):
        """
        Parse a Twisted endpoint description string into an endpoint or
        convert the parse error into a raised L{UsageError}.
        """
        try:
            return serverFromString(self.reactor, description)
        except Exception as e:
            raise UsageError(
                u"Could not parse {description}: {error}".format(
                    description=description,
                    error=str(e),
                    )
                )

    def opt_secure_port(self, endpoint_description):
        """
        A Twisted endpoint description string describing an address at
        which to listen for secure web client connections.  The
        website will be served here.  This option must be used at
        least once.
        """
        endpoint = self._parse_endpoint(endpoint_description)
        self["secure-ports"].append(endpoint)

    def opt_insecure_port(self, endpoint_description):
        """
        A Twisted endpoint description string describing an address at
        which to listen for insecure web client connections.  A
        redirect will be returned sending the client to a secure
        location where the website can be accessed.  This option may
        be used zero or more times.
        """
        endpoint = self._parse_endpoint(endpoint_description)
        self["insecure-ports"].append(endpoint)

    def postOptions(self):
        required_options = [
            "signup-furl-path",
            "interest-path",
            "stripe-secret-api-key-path",
            "stripe-publishable-api-key-path",
            "service-confirmed-path",
            "subscriptions-path",
            "site-logs-path",
        ]
        for option in required_options:
            if self[option] is None:
                raise UsageError("Missing required option --{}".format(option))

        if not self["secure-ports"]:
            raise UsageError(
                u"Use --secure-port at least once to specify an address for "
                u"the website."
            )
        if self["redirect-to-port"] is not None and not self["insecure-ports"]:
            raise UsageError(
                u"Use --insecure-port at least once or there is no server to "
                u"use --redirect-to-port value."
            )

        p = self["site-logs-path"].parent()
        if not p.isdir():
            p.makedirs()


def main(reactor, *argv):
    o = SiteOptions(reactor)
    try:
        o.parseOptions(argv)
    except UsageError as e:
        raise SystemExit(str(e))

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 7s [%(name)-65s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    startLogging(sys.stdout, setStdout=False)

    signup_furl = o["signup-furl-path"].getContent().strip()
    d = start(signup_furl)
    d.addCallback(
        lambda ignored: make_site(
            o["interest-path"],
            o["stripe-secret-api-key-path"].getContent().strip(),
            o["stripe-publishable-api-key-path"].getContent().strip(),
            o["service-confirmed-path"],
            o["subscriptions-path"],
            o["site-logs-path"],
        )
    )
    d.addCallback(
        lambda site: start_site(
            reactor,
            site,
            o["secure-ports"],
            o["insecure-ports"],
            o["redirect-to-port"],
        )
    )
    d.addCallback(lambda ignored: Deferred())
    return d

def start_site(reactor, site, secure_ports, insecure_ports, redirect_to_port):
    parent = MultiService()
    for secure in secure_ports:
        StreamServerEndpointService(secure, site).setServiceParent(parent)

    if insecure_ports:
        redirector = make_redirector_site(redirect_to_port)
        for insecure in insecure_ports:
            StreamServerEndpointService(insecure, redirector).setServiceParent(parent)

    parent.privilegedStartService()
    parent.startService()
