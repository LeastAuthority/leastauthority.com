from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, sys

from lae_automation.aws import queryapi
from lae_automation.signup import signup
from lae_automation.aws.queryapi import time

#to be mocked out
#from lae_automation.signup import install_server, bounce_server, send_signup_confirmation

# Vector data for request responses: activate desktop-, verify-, and describeEC2- responses.
from lae_automation.test.testinitvector import adphttprequestheader, adprequestresponse, verifyhttprequestheader, verifyrequestresponse, describeEC2instresponse


class TestSignupModule(TestCase):
    fakeURLs = [adphttprequestheader, verifyhttprequestheader]
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
        self.mocktxawsS3Clientmakequeryfactory = start_patch('lae_automation.aws.devpay_s3client.DevPayS3Client._make_query_factory')
        self.mockrun_instances = start_patch('lae_automation.initialize.EC2Client.run_instances')
        self.mockrun_instances.return_value = defer.succeed([mock.Mock()])
        self.mockdescribe_instances = start_patch('lae_automation.initialize.EC2Client.describe_instances')
        self.mockdescribe_instances.return_value = defer.succeed( ('0.0.0.0', '0.0.0.0') )

    def tearDown(self):
        [p.__exit__() for p in self._patchers]
 
    def test_signup(self):
        # Arguments to signup
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        MSTDOUT = sys.stdout#StringIO()
        MSTDERR = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'
        CONFIGFILEPATH = '../lae_automation/test/init_test_config.json'
        EC2SECRETPATH = '../lae_automation/test/mock_ec2secret'
        
        def call_time():
            return 0
        self.patch(time, 'time', call_time)

        def call_make_http_request(request_url):
            self.failUnlessEqual(request_url, self.fakeURLs.pop(0))
            return defer.succeed(self.mhr_return_values.pop(0))
        self.patch(queryapi, 'make_http_request', call_make_http_request)            

        su_deferred = signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, MSTDOUT, MSTDERR, MSEED, MSECRETSFILE, CONFIGFILEPATH, EC2SECRETPATH)
        return su_deferred
