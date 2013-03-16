from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock

import lae_automation.initialize
from lae_automation.initialize import activate_user_account_desktop


ACTIVATIONKEY= 'MOCKACTIVATONKEY'
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
USERTOKEN = 'TESTUSERTOKEN'+'A'*385


class InitializationTests (TestCase):
    def _patch_mock(self, obj, attribute):
        m = mock.Mock()
        self.patch(obj, attribute, m)
        return m

    def setUp(self):
        self.mocklsc = self._patch_mock(lae_automation.initialize, 'LicenseServiceClient')


    def test_activate_user_account_desktop(self):
        def call_adpr(activationkey, producttoken):
            mockadpr = mock.Mock(name='ActivateDesktopProductResponse')
            mockadpr.usertoken = USERTOKEN
            return defer.succeed(mockadpr)

        adprcallmock = self.mocklsc.return_value.activate_desktop_product
        adprcallmock.side_effect = call_adpr

        activate_returnval = activate_user_account_desktop(ACTIVATIONKEY,
            PRODUCTTOKEN,
            stdout = StringIO(),
            stderr = StringIO())

        adprcallmock.assert_called_once_with(ACTIVATIONKEY, PRODUCTTOKEN)
        return activate_returnval
