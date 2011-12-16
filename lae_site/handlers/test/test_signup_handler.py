
from StringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.web.http import OK
from twisted.web.util import Redirect

import mock

from lae_site.handlers.signup import SignupHandler
from lae_site.config import Config


SITE_CONFIG_JSON = """{
  "products": [
    { "short_name":    "product",
      "full_name":     "Yummy cloud hotness for everyone",
      "listed":        "true",
      "signup_url":    "https://example.com/abc?offeringCode=12345678",
      "product_code":  "ABCD1234"
    },
    { "short_name":    "product-discount",
      "full_name":     "Cloud delicacy for our bestest friends",
      "listed":        "false",
      "signup_url":    "https://example.com/abc?offeringCode=87654321",
      "product_code":  "DCBA8765"
    }
  ]
}"""

class Handlers(TestCase):
    def test_signuphandler(self):
        config = Config(StringIO(SITE_CONFIG_JSON))
        handler = SignupHandler(config.products)
        (req, resp) = self._mock_request(handler, 'GET')
        req.setResponseCode.assert_called_with(OK)
        self.failUnlessIn("Yummy cloud hotness for everyone", resp)
        self.failUnlessIn('<a href="/signup/product">', resp)
        self.failIfIn("Cloud delicacy for our bestest friends", resp)
        self.failIfIn('<a href="/signup/product-discount">', resp)

        self._check_redirect(handler, 'product', "https://example.com/abc?offeringCode=12345678")
        self._check_redirect(handler, 'product-discount', "https://example.com/abc?offeringCode=87654321")

    def _mock_request(self, handler, method, **args):
        req = mock.Mock(name='HTTPRequest')
        req.method = method
        req.args = args

        resp = handler.render(req)
        return (req, resp)

    def _check_redirect(self, handler, childname, expected_redirect_url):
        req = mock.Mock(name='HTTPRequest')
        req.method = 'GET'
        req.args = []

        r = handler.getChildWithDefault(childname, req)
        self.failUnless(isinstance(r, Redirect))
        self.failUnless(r.url, expected_redirect_url)
