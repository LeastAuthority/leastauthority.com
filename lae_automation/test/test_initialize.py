
import sys
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer
from twisted.python.failure import Failure

import mock

import lae_automation.initialize
from lae_automation.initialize import activate_user_account_desktop, \
    verify_and_store_serverssh_pubkey, PublicKeyMismatch


ACTIVATIONKEY= 'MOCKACTIVATONKEY'
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
USERTOKEN = 'TESTUSERTOKEN'+'A'*385


class InitializationTests (TestCase):
    def _patch_mock(self, obj, attribute):
        m = mock.Mock()
        self.patch(obj, attribute, m)
        return m

    def setUp(self):
        self.mockwaitfor_console = self._patch_mock(lae_automation.initialize, 'wait_for_EC2_sshfp')
        self.mockwaitfor_EC2prop = self._patch_mock(lae_automation.initialize, 'wait_for_EC2_properties')
        self.mockwaitfor_keyscan = self._patch_mock(lae_automation.initialize, 'wait_for_and_store_pubkeyfp_from_keyscan')
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

    def test_verify_and_store_serverssh_pubkey(self):
        self.mockwaitfor_console.side_effect = lambda *args: defer.succeed('NONMATCHING FINGERPRINT_FROM_CONSOLE')
        self.mockwaitfor_EC2prop.side_effect = lambda *args: defer.succeed((('IPfromAWS', 'XXX'), 'XXX'))
        self.mockwaitfor_keyscan.return_value = ('NONMATCHING FINGERPRINT_FROM_KEYSCAN', 'HASHED_PUBKEY', 'IPfromAWS')

        mismatchfailure = Failure(PublicKeyMismatch)
        argtuple = ('ec2accesskeyid', 'ec2secretkey', 'endpoint_uri', 'addressparser',
                    5, 15, sys.stdout, sys.stderr, 'instance_id')
        d = self.failUnlessFailure(verify_and_store_serverssh_pubkey(*argtuple),
                                   mismatchfailure.value)
        return d
