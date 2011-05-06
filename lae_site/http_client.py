from urlparse import urlparse

from twisted.internet import reactor, ssl
from twisted.web.client import HTTPClientFactory


def make_http_request(url, method='GET', reactor=reactor):
    """
    This is a thin wrapper around twisted HTTPClientFactory and
    reactor.connect(TCP/SSL).

    Code which makes HTTP client requests should accept a parameter
    which provides this interface for all HTTP connections, defaulting
    to this function.  This allows unittests to inject a fake http
    request mechanism.

    @param url: The url to request.
    @param method: The HTTP request method.
    @param reactor: Anything providing the IReactorSSL interface.

    @return: A L{Deferred} which fires with the response body.
    """
    client = HTTPClientFactory(url, method='POST')

    pr = urlparse(url)

    if pr.scheme == 'https':
        port = pr.port or 443
        contextFactory = ssl.ClientContextFactory()
        reactor.connectSSL(pr.hostname, port, client, contextFactory)
    else:
        port = pr.port or 80
        reactor.connectTCP(pr.hostname, port, client)

    return client.deferred
