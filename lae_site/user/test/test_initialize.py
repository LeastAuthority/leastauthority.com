from twisted.trial.unittest import TestCase
from twisted.internet.defer import Deferred
from twisted.internet import reactor

import mock

from lae_site.user.initialize import activate_user_account_desktop, activate_user_account_hosted


class InitializationTests (TestCase):

    def setUp(self):
        self._patchers = []
        def start_patch(name):
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.__enter__()

        self.mocklsc = start_patch('lae_site.user.initialize.LicenseServiceClient')
        self.mocks3c = start_patch('lae_site.user.initialize.DevPayS3Client')

    def tearDown(self):
        [p.__exit__() for p in self._patchers]

    def test_activate_user_account_desktop(self):
        def make_deferred_fire_factory(value):
            d = Deferred()
            reactor.callLater(0, d.callback, value)
            return d

        mockadpr = mock.Mock(name='ActivateDesktopProductResponse')
        mockadpr.usertoken = "{UserToken}..."

        self.mocklsc.return_value.activate_desktop_product.return_value = make_deferred_fire_factory(mockadpr)

        self.mocks3c.return_value.create_bucket.return_value = make_deferred_fire_factory(mock.sentinel.UNKNOWN)

        mockstatus = mock.Mock(name='StatusCallback')

        return activate_user_account_desktop(
            activationkey = mock.sentinel.activationkey,
            producttoken = mock.sentinel.producttoken,
            status_callback = mockstatus)

    def test_activate_user_account_hosted(self):
        def make_deferred_fire_factory(value):
            d = Deferred()
            reactor.callLater(0, d.callback, value)
            return d

        mockahpr = mock.Mock(name='ActivateHostedProductResponse')
        mockahpr.usertoken = "{UserToken}..."
        mockahpr.pid = "PID"

        self.mocklsc.return_value.activate_hosted_product.return_value = make_deferred_fire_factory(mockahpr)

        self.mocks3c.return_value.create_bucket.return_value = make_deferred_fire_factory(mock.sentinel.UNKNOWN)

        mockcreds = mock.Mock(name='Credentials')
        mockcreds.sign.return_value = mock.sentinel.SIGNED_CREDS

        mockstatus = mock.Mock(name='StatusCallback')

        return activate_user_account_hosted(
            creds = mockcreds,
            activationkey = mock.sentinel.activationkey,
            producttoken = mock.sentinel.producttoken,
            status_callback = mockstatus)


