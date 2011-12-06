
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, os, sys
print mock

from lae_automation.initialize import activate_user_account_desktop
from lae_automation.signup import signup


class TestMakeHTTPRequest(TestCase):
    def setUp(self):
        self.original_directory = os.getcwd()
        curdir = self.original_directory.split(os.sep)[-1]
        if curdir == '_trial_temp':
            os.chdir('../')# Hack to work-around trial resetting cwd.
        self._patchers = []
        def start_patch(name):
            print "The the start_path function patching name: %s"%name
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.__enter__()

        self.mockmhr = start_patch('lae_automation.aws.queryapi.make_http_request')

    def tearDown(self):
        os.chdir(self.original_directory)
        [p.__exit__() for p in self._patchers]

    def test_initialization(self):
        mactivationkey = 'MOCKACTIVATONKEY'
        mproductcode = 'ABCDEFGH'
        mname = 'MNAME'
        memail = 'MEMAIL'
        mkeyinfo = 'MKEYINFO'
        mstdout = sys.stdout#StringIO()
        mstderr = sys.stderr#StringIO()
        mseed = 'MSEED'
        msecretsfile = 'MSECRETSFILE' 
        signup(mactivationkey, mproductcode, mname, memail, mkeyinfo, mstdout, mstderr, mseed, msecretsfile)
        print dir(self.mockmhr)
        print "self.mockmhr._name: %s"%self.mockmhr._name
        print "self.mockmhr.call_count: %s"%self.mockmhr.call_count
        print "self.mockmhr.called: %s"%self.mockmhr.called
        print "type(self.mockmhr): %s"%type(self.mockmhr)


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

