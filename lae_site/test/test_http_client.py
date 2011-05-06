from twisted.trial.unittest import TestCase
from twisted.web.client import HTTPClientFactory
from twisted.internet.defer import Deferred
from twisted.internet import ssl

from lae_site.http_client import make_http_request


class HttpClientTests (TestCase):

    def test_make_http_request(self):

        d = make_http_request(
            url='http://foo.faketld/banana?wombat',
            reactor=MockReactor(
                self.assertEqual,
                'connectTCP',
                'foo.faketld',
                80))

        self.failUnless( isinstance( d, Deferred ) )


    def test_make_http_request_with_port(self):

        d = make_http_request(
            url='http://bar.faketld:1234/banana?wombat',
            reactor=MockReactor(
                self.assertEqual,
                'connectTCP',
                'bar.faketld',
                1234))

        self.failUnless( isinstance( d, Deferred ) )


    def test_make_https_request(self):

        d = make_http_request(
            url='https://secure-foo.faketld/banana?wombat',
            reactor=MockReactor(
                self.assertEqual,
                'connectSSL',
                'secure-foo.faketld',
                443))

        self.failUnless( isinstance( d, Deferred ) )


    def test_make_https_request_with_port(self):

        d = make_http_request(
            url='https://secure-bar.faketld:1234/banana?wombat',
            reactor=MockReactor(
                self.assertEqual,
                'connectSSL',
                'secure-bar.faketld',
                1234))

        self.failUnless( isinstance( d, Deferred ) )


class MockReactor (object):

    __slots__ = [ '_assertEqual', '_methodname', '_host', '_port' ]

    def __init__(self, assertEqual, methodname, host, port):
        self._assertEqual = assertEqual
        self._methodname = methodname
        self._host = host
        self._port = port

    def connectTCP(self, host, port, factory):
        self._assertEqual( (self._methodname, self._host, self._port), ('connectTCP', host, port) )
        self._assertEqual( True, isinstance(factory, HTTPClientFactory) )

    def connectSSL(self, host, port, factory, sslfactory):
        self._assertEqual( (self._methodname, self._host, self._port), ('connectSSL', host, port) )
        self._assertEqual( True, isinstance(factory, HTTPClientFactory) )
        self._assertEqual( True, isinstance(sslfactory, ssl.ClientContextFactory) )
