
import string
from cStringIO import StringIO

import mock

from twisted.python.filepath import FilePath
from twisted.trial.unittest import TestCase
from twisted.web.http import OK, NOT_FOUND
from twisted.web.server import unquote

from lae_site.config import Config
from lae_site.handlers import make_site


SITE_CONFIG_JSON = """{ "products": [] }"""

class Site(TestCase):
    def test_site(self):
        config = Config(StringIO(SITE_CONFIG_JSON))
        site = make_site(FilePath('.'), config)

        (req, resp) = self._mock_request(site, '/', 'GET')
        req.setResponseCode.assert_called_with(OK)
        self.failUnlessIn("<em>provider-independent security</em>", resp)

        (req, resp) = self._mock_request(site, '/support', 'GET')
        req.setResponseCode.assert_called_with(OK)
        self.failUnlessIn("<h3>Product Support</h3>", resp)

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
