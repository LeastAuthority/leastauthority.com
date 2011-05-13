from twisted.trial.unittest import TestCase
from twisted.web.http import OK, BAD_REQUEST

import mock

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler


class DevPayPurchaseHandlerTests (TestCase):

    def setUp(self):
        self.dpph = DevPayPurchaseHandler()


    def test_valid_request(self):

        akey = mock.sentinel.ActivationKey
        pcode = mock.sentinel.ProductCode

        (req, _) = self._mock_request('GET', ActivationKey = [akey], ProductCode = [pcode])

        req.setResponseCode.assert_called_with(OK)


    def test_POST(self):

        (req, _) = self._mock_request('POST')

        req.setResponseCode.assert_called_with(BAD_REQUEST)


    def test_missing_required_params(self):

        (req, _) = self._mock_request('GET')

        req.setResponseCode.assert_called_with(BAD_REQUEST)


    def test_malformed_params(self):

        S = mock.sentinel

        (req, _) = self._mock_request('GET', ActivationKey = [S.ActivationKey])
        req.setResponseCode.assert_called_with(BAD_REQUEST)

        (req, _) = self._mock_request('GET', ProductCode = [S.ProductCode])
        req.setResponseCode.assert_called_with(BAD_REQUEST)

        (req, _) = self._mock_request('GET', ActivationKey = [S.ActivationKey], ProductCode = [])
        req.setResponseCode.assert_called_with(BAD_REQUEST)

        (req, _) = self._mock_request('GET', ActivationKey = [], ProductCode = [S.ProductCode])
        req.setResponseCode.assert_called_with(BAD_REQUEST)

        (req, _) = self._mock_request(
            'GET',
            ActivationKey = [S.ActivationKey, S.OtherActivationKey],
            ProductCode = [S.ProductCode])
        req.setResponseCode.assert_called_with(BAD_REQUEST)

        (req, _) = self._mock_request(
            'GET',
            ActivationKey = [S.ActivationKey],
            ProductCode = [S.ProductCode, S.OtherProductCode])
        req.setResponseCode.assert_called_with(BAD_REQUEST)


    def _mock_request(self, method, **args):

        req = mock.Mock(name='HTTPRequest')
        req.method = method
        req.args = args

        resp = self.dpph.render(req)

        return (req, resp)

