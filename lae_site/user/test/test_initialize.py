from twisted.trial.unittest import TestCase
from twisted.internet.defer import Deferred
from twisted.internet import reactor

import mock

from lae_site.user.initialize import initialize_user_account


class InitializationTests (TestCase):

    @mock.patch('lae_site.user.initialize.LicenseServiceClient')
    @mock.patch('lae_site.user.initialize.DevPayS3Client')
    def test_initialize_user_account(self, mocklsc, mocks3c):

        def make_deferred_fire_factory(value):
            d = Deferred()
            reactor.callLater(0, d.callback, value)
            return d

        mockahpr = mock.Mock(name='ActivateHostedProductResponse')
        mockahpr.usertoken = mock.sentinel.UserToken

        mocklsc.return_value.activate_hosted_product.return_value = make_deferred_fire_factory(mockahpr)

        mocks3c.return_value.create_bucket.return_value = make_deferred_fire_factory(mock.sentinel.UNKNOWN)

        mockstatus = mock.Mock(name='StatusCallback')

        return initialize_user_account(
            creds = mock.sentinel.creds,
            activationkey = mock.sentinel.activationkey,
            status_callback = mockstatus)




