from twisted.trial.unittest import TestCase
from twisted.internet.defer import Deferred
from twisted.internet import reactor

import mock

from lae_site.user.initialize import initialize_user_account


class InitializationTests (TestCase):

    def setUp(self):
        self._patchers = []
        def start_patch(name):
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.start()

        self.mocklsc = start_patch('lae_site.user.initialize.LicenseServiceClient')
        self.mocks3c = start_patch('lae_site.user.initialize.DevPayS3Client')
        self.mocketok = start_patch('lae_site.user.initialize.EntropicToken')

    def tearDown(self):
        [p.stop() for p in self._patchers]

    def test_initialize_user_account(self):

        def make_deferred_fire_factory(value):
            d = Deferred()
            reactor.callLater(0, d.callback, value)
            return d

        mockahpr = mock.Mock(name='ActivateHostedProductResponse')
        mockahpr.usertoken = mock.sentinel.UserToken

        self.mocklsc.return_value.activate_hosted_product.return_value = make_deferred_fire_factory(mockahpr)

        self.mocks3c.return_value.create_bucket.return_value = make_deferred_fire_factory(mock.sentinel.UNKNOWN)

        mockcreds = mock.Mock(name='Credentials')
        mockcreds.sign.return_value = mock.sentinel.SIGNED_CREDS

        mockstatus = mock.Mock(name='StatusCallback')

        return initialize_user_account(
            creds = mockcreds,
            activationkey = mock.sentinel.activationkey,
            status_callback = mockstatus)


