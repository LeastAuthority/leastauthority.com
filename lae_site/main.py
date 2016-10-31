# before importing Twisted
import mimetypes
mimetypes.add_type("text/plain", ".rst")

if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    from lae_site.main import main

    react(main, argv[1:])

import sys, os
import logging

import pem

from twisted.internet import ssl
from twisted.internet.defer import Deferred
from twisted.python.usage import UsageError, Options
from twisted.python.filepath import FilePath

from lae_site.handlers import make_site, make_redirector_site
from lae_site.handlers.submit_subscription import start

root_log = logging.getLogger(__name__)

class SiteOptions(Options):
    optFlags = [
        ("noredirect", None, "Disable the cleartext redirect-to-TLS site."),
        ("nossl", None, "Run the site on a cleartext HTTP server instead of over TLS. "),
    ]

    optParameters = [
        ("signup-furl-path", None, None, "A path to a file containing the signup furl.", FilePath),
        ("interest-path", None, None, "A path to a file to which contact information of people interested in products will be appended.", FilePath),
        ("stripe-api-key-path", None, None, "A path to a file containing a Stripe API key.", FilePath),
        ("subscriptions-path", None, None, "A path to a file to which new subscription details will be appended.", FilePath),
        ("service-confirmed-path", None, None, "A path to a file to which confirmed-service subscription details will be appended.", FilePath),
        ("site-logs-path", None, None, "A path to a file to which HTTP logs for the site will be written.", FilePath),

        ("port", None, "443", "The TCP port number on which to listen for TLS/HTTP requests.", int),
        ("redirectport", "80", None, "A TCP port number on which to run a redirect-to-TLS site.", int),
    ]


    def postOptions(self):
        required_options = [
            "signup-furl-path",
            "interest-path",
            "stripe-api-key-path",
            "service-confirmed-path",
            "subscriptions-path",
            "site-logs-path",
        ]
        for option in required_options:
            if self[option] is None:
                raise UsageError("Missing required option --{}".format(option))


def main(reactor, *argv):
    o = SiteOptions()
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

    root_log.info('Listening on port {}...'.format(o["port"]))

    d = start(o["signup-furl-path"])
    d.addCallback(
        lambda ignored: make_site(
            o["email-path"],
            o["stripe-api-key-path"].getContent().strip(),
            o["service-confirmed-path"],
            o["subscriptions-path"],
            o["site-logs-path"],
        )
    )
    d.addCallback(
        lambda site: start_site(
            reactor,
            site,
            o["port"],
            not o["nossl"],
            not o["noredirect"], o["redirectport"],
        )
    )
    d.addCallback(lambda ignored: Deferred())
    return d

def start_site(reactor, site, port, ssl_enabled, redirect, redirect_port):
    if ssl_enabled:
        root_log.info('SSL/TLS is enabled (start with --nossl to disable).')
        KEYFILE = '../secret_config/rapidssl/server.key'
        CERTFILE = '../secret_config/rapidssl/server.crt'
        assert os.path.exists(KEYFILE), "Private key file %s not found" % (KEYFILE,)
        assert os.path.exists(CERTFILE), "Certificate file %s not found" % (CERTFILE,)

        with open(KEYFILE) as keyFile:
            key = keyFile.read()

        certs = pem.parse_file(CERTFILE)
        cert = ssl.PrivateCertificate.loadPEM(str(key) + str(certs[0]))

        extraCertChain = [ssl.Certificate.loadPEM(str(certData)).original
                          for certData in certs[1:]]

        cert_options = ssl.CertificateOptions(
            privateKey=cert.privateKey.original,
            certificate=cert.original,
            extraCertChain=extraCertChain,
        )

        reactor.listenSSL(port, site, cert_options)

        if redirect:
            root_log.info('http->https redirector listening on port %d...' % (redirect_port,))
            reactor.listenTCP(redirect_port, make_redirector_site(port))
    else:
        root_log.info('SSL/TLS is disabled.')
        reactor.listenTCP(port, site)
