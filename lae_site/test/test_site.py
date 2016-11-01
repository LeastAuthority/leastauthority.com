
import string

import mock

from twisted.python.filepath import FilePath
from twisted.trial.unittest import TestCase
from twisted.web.http import OK, NOT_FOUND
from twisted.web.server import unquote

from lae_site.handlers import make_site


SITE_CONFIG_JSON = """{}"""
STRIPE_PUBLISHABLE_API_KEY = b"abcdef"
STRIPE_API_KEY = b"abcdef"

class Site(TestCase):
    def test_site(self):
        root = FilePath(self.mktemp())
        site = make_site(
            root.child(b"emails.csv"),
            STRIPE_API_KEY,
            STRIPE_PUBLISHABLE_API_KEY,
            root.child(b"confirmed.csv"),
            root.child(b"subscriptions.csv"),
            root.child("sitelogs"),
        )

        (req, resp) = self._mock_request(site, '/', 'GET')
        req.setResponseCode.assert_called_with(OK)
        self.failUnlessIn("Simple, secure", resp)

        (req, resp) = self._mock_request(site, '/noexist', 'GET')
        req.setResponseCode.assert_called_with(NOT_FOUND)

    def _mock_request(self, site, path, method, **args):
        req = mock.Mock(name='HTTPRequest')
        req.path = path
        req.prepath = []
        req.postpath = map(unquote, string.split(path[1:], '/'))
        req.method = method
        req.args = args

        resource = site.getResourceFor(req)
        resp = resource.render(req)
        return (req, resp)
