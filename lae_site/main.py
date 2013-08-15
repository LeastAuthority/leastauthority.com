
import sys, os, mimetypes
import logging

# before importing Twisted
mimetypes.add_type("text/plain", ".rst")


from twisted.internet import reactor
from twisted.python.filepath import FilePath

from lae_site.config import Config
from lae_site.handlers import make_site, make_redirector_site
from lae_site.handlers.submit_subscription import start


def main(basefp):
    print sys.argv
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
        format = '%(asctime)s %(levelname) 7s [%(name)-65s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z',
        )

    root_log = logging.getLogger(__name__)

    site = make_site(basefp, config)

    root_log.info('Listening on port %d...' % (port,))
    if ssl_enabled:
        root_log.info('SSL/TLS is enabled (start with --nossl to disable).')
        KEYFILE = '../secret_config/rapidssl/server.key'
        CERTFILE = '../secret_config/rapidssl/server.crt'
        assert os.path.exists(KEYFILE), "Private key file %s not found" % (KEYFILE,)
        assert os.path.exists(CERTFILE), "Certificate file %s not found" % (CERTFILE,)

        from twisted.internet.ssl import DefaultOpenSSLContextFactory
        #from OpenSSL.SSL import OP_SINGLE_DH_USE

        # <http://www.openssl.org/docs/ssl/SSL_CTX_set_options.html#NOTES>
        # <https://github.com/openssl/openssl/blob/6f017a8f9db3a79f3a3406cf8d493ccd346db691/ssl/ssl.h#L656>
        OP_CIPHER_SERVER_PREFERENCE = 0x00400000L

        CIPHER_LIST = ("ECDHE-RSA-AES128-GCM-SHA256:"
                       "ECDHE-RSA-AES256-GCM-SHA384:"
                       "DHE-RSA-AES128-GCM-SHA256:"
                       "DHE-RSA-AES256-GCM-SHA384:"
                       "ECDHE-RSA-AES128-SHA256:"
                       "DHE-RSA-AES128-SHA256:"
                       "DHE-RSA-AES256-SHA256:"
                       "ECDHE-RSA-AES128-SHA:"
                       "ECDHE-RSA-AES256-SHA:"
                       "DHE-RSA-AES128-SHA;"
                       "DHE-RSA-AES256-SHA:"
                       "ECDHE-RSA-DES-CBC3-SHA:"
                       "AES128-GCM-SHA256:"
                       "AES256-GCM-SHA384:"
                       "AES128-SHA256:"
                       "AES256-SHA256:"
                       "AES128-SHA:"
                       "AES256-SHA:"
                       "DES-CBC3-SHA:"
                       # RC4 at the bottom, even if forward-secret
                       "ECDHE-RSA-RC4-SHA;"
                       "RC4-SHA;")

        # http://twistedmatrix.com/documents/current/core/howto/ssl.html
        sslfactory = DefaultOpenSSLContextFactory(KEYFILE, CERTFILE)
        sslcontext = sslfactory.getContext()
        sslcontext.set_cipher_list(CIPHER_LIST)
        #sslcontext.set_options(OP_SINGLE_DH_USE)
        sslcontext.set_options(OP_CIPHER_SERVER_PREFERENCE)
        reactor.listenSSL(port, site, sslfactory)

        if redirect_port is not None:
            root_log.info('http->https redirector listening on port %d...' % (redirect_port,))
            reactor.listenTCP(redirect_port, make_redirector_site(port))
    else:
        root_log.info('SSL/TLS is disabled.')
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
