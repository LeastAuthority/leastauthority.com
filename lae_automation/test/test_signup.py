from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, sys

from lae_automation.signup import signup

# Vector data for request responses: activate desktop-, verify-, and describeEC2- responses.
from lae_automation.test.testinitvector import adprequestresponse, verifyrequestresponse, describeEC2instresponse


class TestSignupModule(TestCase):
    mhr_return_values = [adprequestresponse, verifyrequestresponse, describeEC2instresponse]
    def setUp(self):
        self._patchers = []
        def start_patch(name):
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.__enter__()
        self.mockinstall_server = start_patch('lae_automation.signup.install_server')
        self.mockbounce_server = start_patch('lae_automation.signup.bounce_server')
        self.mocksend_confirmation = start_patch('lae_automation.signup.send_signup_confirmation')
        self.mockmhr = start_patch('lae_automation.aws.queryapi.make_http_request')
        self.mockmhr.side_effect = self.mockmakehttprequestreturns
        self.mocktxawsS3Clientmakequeryfactory = start_patch('lae_automation.aws.devpay_s3client.DevPayS3Client._make_query_factory')
        self.mockrun_instances = start_patch('lae_automation.initialize.EC2Client.run_instances')
        self.mockrun_instances.return_value = defer.succeed([mock.Mock()])
        self.mockdescribe_instances = start_patch('lae_automation.initialize.EC2Client.describe_instances')
        self.mockdescribe_instances.return_value = defer.succeed( ('0.0.0.0', '0.0.0.0') )

    def tearDown(self):
        [p.__exit__() for p in self._patchers]
 
    def mockmakehttprequestreturns(self, argument_to_make_http_request):
        return defer.succeed(self.mhr_return_values.pop(0))

    def test_signup(self):
        # Arguments to signup
        mactivationkey = 'MOCKACTIVATONKEY'
        mproductcode = 'ABCDEFGH'
        mname = 'MNAME'
        memail = 'MEMAIL'
        mkeyinfo = 'MKEYINFO'
        mstdout = sys.stdout#StringIO()
        mstderr = StringIO()
        mseed = 'MSEED'
        msecretsfile = 'MSECRETSFILE'
        configfilepath = '../lae_automation/test/init_test_config.json'
        ec2secretpath = '../../ec2secret'
        su_deferred = signup(mactivationkey, mproductcode, mname, memail, mkeyinfo, mstdout, mstderr, mseed, msecretsfile, configfilepath, ec2secretpath )
        return su_deferred
