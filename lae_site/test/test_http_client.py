from twisted.trial.unittest import TestCase

import mock

from lae_site.http_client import make_http_request


class HttpClientTests (TestCase):

    @mock.patch('lae_site.http_client.HTTPClientFactory')
    @mock.patch('lae_site.http_client.reactor')
    def test_make_http_request(self, mockreactor, mockcfac):

        d = make_http_request('http://foo.faketld/banana?wombat')

        self.assertEqual(d, mockcfac.return_value.deferred)
        self.assertEqual(
            mockreactor.method_calls,
            [('connectTCP', ('foo.faketld', 80, mockcfac.return_value))])


    @mock.patch('lae_site.http_client.HTTPClientFactory')
    @mock.patch('lae_site.http_client.reactor')
    def test_make_http_request_with_port(self, mockreactor, mockcfac):

        d = make_http_request('http://bar.faketld:1234/banana?wombat')

        self.assertEqual(d, mockcfac.return_value.deferred)
        self.assertEqual(
            mockreactor.method_calls,
            [('connectTCP', ('bar.faketld', 1234, mockcfac.return_value))])


    @mock.patch('lae_site.http_client.ssl')
    @mock.patch('lae_site.http_client.HTTPClientFactory')
    @mock.patch('lae_site.http_client.reactor')
    def test_make_https_request(self, mockreactor, mockcfac, mockssl):

        d = make_http_request('https://secure-foo.faketld/banana?wombat')

        self.assertEqual(d, mockcfac.return_value.deferred)
        self.assertEqual(
            mockreactor.method_calls,
            [('connectSSL',
              ('secure-foo.faketld',
               443,
               mockcfac.return_value,
               mockssl.ClientContextFactory.return_value))])


    @mock.patch('lae_site.http_client.ssl')
    @mock.patch('lae_site.http_client.HTTPClientFactory')
    @mock.patch('lae_site.http_client.reactor')
    def test_make_https_request_with_port(self, mockreactor, mockcfac, mockssl):

        d = make_http_request('https://secure-bar.faketld:1234/banana?wombat')

        self.assertEqual(d, mockcfac.return_value.deferred)
        self.assertEqual(
            mockreactor.method_calls,
            [('connectSSL',
              ('secure-bar.faketld',
               1234,
               mockcfac.return_value,
               mockssl.ClientContextFactory.return_value))])
