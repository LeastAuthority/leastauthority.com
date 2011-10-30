#! /usr/bin/env python

import sys, os
import logging

from twisted.internet import ssl, reactor

from lae_site.config import Config
from lae_site.handlers import make_site, make_redirector_site

# Test comment by sirvaliance, remove me
def main():
    default_port = 443
    port = None
    ssl_enabled = True
    redirect_to_https = True

    for arg in sys.argv:
        if arg.startswith('--port='):
            port = int(arg[len('--port='):])
        elif arg == '--nossl':
            ssl_enabled = False
            redirect_to_https = False
            default_port = 80
        elif arg == '--noredirect':
            redirect_to_https = False

    if port is None:
        port = default_port

    config = Config()

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 7s [%(module) 8s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    if config.unknown_options:
        logging.warn('Unknown options: %r', config.unknown_options.keys())

    site = make_site(config)

    logging.info('Listening on port %d...' % port)
    if ssl_enabled:
        logging.info('SSL/TLS is enabled (start with --nossl to disable).')
        KEYFILE = 'keys/server.key'
        CERTFILE = 'keys/server.crt'
        assert os.path.exists(KEYFILE), "Private key file %s not found" % (KEYFILE,)
        assert os.path.exists(CERTFILE), "Certificate file %s not found" % (CERTFILE,)

        # http://twistedmatrix.com/documents/current/core/howto/ssl.html
        sslcontext = ssl.DefaultOpenSSLContextFactory(KEYFILE, CERTFILE).getContext()
        reactor.listenSSL(port, site, sslcontext)

        if redirect_to_https:
            reactor.listenTCP(80, make_redirector_site())
    else:
        logging.info('SSL/TLS is disabled.')
        reactor.listenTCP(port, site)
    reactor.run()


if __name__ == '__main__':
    main()
