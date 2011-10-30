
from StringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.web.http import OK
from twisted.python.filepath import FilePath

import mock

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler, ActivationRequestHandler


class Handlers(TestCase):
    def test_devpaypurchasehandler(self):
        out = StringIO()
        for method in ('GET', 'POST'):
            (req, resp) = self._mock_request(DevPayPurchaseHandler(out), method,
                                             ActivationKey=["ACTIVATIONKEY"], ProductCode=["PRODUCTCODE"])

            req.setResponseCode.assert_called_with(OK)
            self.failUnlessIn('name="ActivationKey" value="ACTIVATIONKEY"', resp)
            self.failUnlessIn('name="ProductCode" value="PRODUCTCODE"', resp)
            csv = FilePath("devpay_completions.csv").getContent()
            self.failUnlessIn("ACTIVATIONKEY,PRODUCTCODE\n", csv)

    def test_activationrequesthandler(self):
        out = StringIO()
        for method in ('GET', 'POST'):
            (req, resp) = self._mock_request(ActivationRequestHandler(out), method,
                                             ActivationKey=["ACTIVATIONKEY"], ProductCode=["PRODUCTCODE"],
                                             Name=["Joe"], Email=["joe!@example.org"], PublicKey=["===BEGIN BLAH==="])

            req.setResponseCode.assert_called_with(OK)
            csv = FilePath("activation_requests.csv").getContent()
            self.failUnlessIn("ACTIVATIONKEY,PRODUCTCODE,Joe,joe%21@example.org,===BEGIN BLAH===\n", csv)

    def _mock_request(self, handler, method, **args):
        req = mock.Mock(name='HTTPRequest')
        req.method = method
        req.args = args

        resp = handler.render(req)
        return (req, resp)
