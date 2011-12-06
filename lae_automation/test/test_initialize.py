
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock

from lae_automation.initialize import activate_user_account_desktop
from lae_automation.initialize import deploy_EC2_instance

class InitializationTests (TestCase):

    def setUp(self):
        self._patchers = []
        def start_patch(name):
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.__enter__()

        self.mocklsc = start_patch('lae_automation.initialize.LicenseServiceClient')

    def tearDown(self):
        [p.__exit__() for p in self._patchers]

    def test_activate_user_account_desktop(self):
        def make_deferred_fire_factory(value):
            return defer.succeed(value)

        mockadpr = mock.Mock(name='ActivateDesktopProductResponse')
        mockadpr.usertoken = "{UserToken}..."

        self.mocklsc.return_value.activate_desktop_product.return_value = make_deferred_fire_factory(mockadpr)

        return activate_user_account_desktop(
            activationkey = mock.sentinel.activationkey,
            producttoken = mock.sentinel.producttoken,
            stdout = StringIO(),
            stderr = StringIO())

    def test_deploy_EC2_instance(self):
        return deploy_EC2_instance(
            creds = ,
            endpoint_uri = ,
            ami_image_id = ,
            instance_size = ,
            bucket_name = ,
            keypair_name = ,
            stdout = ,
            stderr = )

