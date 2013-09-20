#! /usr/bin/env python

import sys, os, mimetypes
import logging

# before importing Twisted
mimetypes.add_type("text/plain", ".rst")


from twisted.internet import ssl, reactor
from twisted.python.filepath import FilePath

from lae_site.config import Config
from lae_site.handlers import make_site, make_redirector_site
from lae_site.handlers.devpay_complete import start


def main(basefp):
    default_port = 443
    port = None
    ssl_enabled = True
    redirect_port = 80

    for arg in sys.argv:
        if arg.startswith('--port='):
            port = int(arg[len('--port='):])
        elif arg.startswith('--redirectport='):
            redirect_port = int(arg[len('--redirectport='):])
        elif arg == '--dev':
            ssl_enabled = False
            redirect_port = None
            default_port = 8000
        elif arg == '--nossl':
            ssl_enabled = False
            redirect_port = None
            default_port = 80
        elif arg == '--noredirect':
            redirect_port = None

    if port is None:
        port = default_port

    config = Config()

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 7s [%(module) 8s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    site = make_site(basefp, config)

    logging.info('Listening on port %d...' % (port,))
    if ssl_enabled:
        logging.info('SSL/TLS is enabled (start with --nossl to disable).')
        KEYFILE = '../secret_config/rapidssl/server.key'
        CERTFILE = '../secret_config/rapidssl/server.crt'
        assert os.path.exists(KEYFILE), "Private key file %s not found" % (KEYFILE,)
        assert os.path.exists(CERTFILE), "Certificate file %s not found" % (CERTFILE,)

        # http://twistedmatrix.com/documents/current/core/howto/ssl.html
        sslfactory = ssl.DefaultOpenSSLContextFactory(KEYFILE, CERTFILE)
        reactor.listenSSL(port, site, sslfactory)

        if redirect_port is not None:
            logging.info('http->https redirector listening on port %d...' % (redirect_port,))
            reactor.listenTCP(redirect_port, make_redirector_site(port))
    else:
        logging.info('SSL/TLS is disabled.')
        reactor.listenTCP(port, site)


if __name__ == '__main__':
    basefp = FilePath('..')
    def _err(f):
        print f
        return f

    d = start(basefp)
    d.addCallback(lambda ign: main(basefp))
    d.addErrback(_err)
    d.addErrback(lambda ign: os._exit(1))
    reactor.run()
