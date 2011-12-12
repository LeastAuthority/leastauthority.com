
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, os, sys

from lae_automation.initialize import activate_user_account_desktop
from lae_automation.signup import signup


class TestAutoSetup(TestCase):
    def setUp(self):
        # Elements of activation request response.
        mutoken = 'TEST'+'A'*394
        makeyID = 'TEST'+'A'*16
        msecakey = 'TEST'+'A'*36
        mrequestid = 'TEST'+'A'*32
        # The Activation Request Response
        self.adprequestresponse = """<ActivateDesktopProductResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><ActivateDesktopProductResult><UserToken>{UserToken}%s==</UserToken><AWSAccessKeyId>%s</AWSAccessKeyId><SecretAccessKey>%s</SecretAccessKey></ActivateDesktopProductResult><ResponseMetadata><RequestId>%s</RequestId></ResponseMetadata></ActivateDesktopProductResponse>"""%(mutoken, makeyID, msecakey, mrequestid)

        self.verifyrequestresponse = """<VerifyProductSubscriptionByTokensResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><VerifyProductSubscriptionByTokensResult><Subscribed>true</Subscribed></VerifyProductSubscriptionByTokensResult><ResponseMetadata><RequestId>bd9db94b-a1b0-4a5f-8d70-6cc4de427623</RequestId></ResponseMetadata></VerifyProductSubscriptionByTokensResponse>"""

        self.describeEC2instresponse = """<?xml version="1.0" encoding="UTF-8"?><DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/"><requestId>TEST</requestId><reservationSet><item><reservationId>TEST</reservationId><ownerId>TEST</ownerId><groupSet><item><groupId>CustomerDefault</groupId></item></groupSet><instancesSet><item><instanceId>TEST</instanceId><imageId>TEST</imageId><instanceState><code>TEST</code><name>TEST</name></instanceState><privateDnsName>TESTinternal</privateDnsName><dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName><reason/><keyName>TEST</keyName><amiLaunchIndex>0</amiLaunchIndex><productCodes/><instanceType>t1.TEST</instanceType><launchTime>TEST</launchTime><placement><availabilityZone>TEST</availabilityZone></placement><kernelId>TEST</kernelId></item></instancesSet></item></reservationSet></DescribeInstancesResponse>""" 
        self.mhr_return_values = [self.adprequestresponse, self.verifyrequestresponse, self.describeEC2instresponse]
        self.run_return_values = ['ubuntu']

        self.original_directory = os.getcwd()
        curdir = self.original_directory.split(os.sep)[-1]
        if curdir == '_trial_temp':
            os.chdir('../')# Hack to work-around trial resetting cwd.
        self._patchers = []

        def start_patch(name):
            patcher = mock.patch(name)
            self._patchers.append(patcher)
            return patcher.__enter__()

        self.mockfabapi = start_patch('lae_automation.server.api')            
        self.mockfabapi.run.side_effect = self.runsideeffects
        self.mockfabcd = start_patch('lae_automation.server.cd')
        self.mockfabcd.__enter__ = mock.Mock()
        self.mockfabcd.__exit__ = mock.Mock()
        self.mockfabcd.__exit__.return_value = None, None, None
        self.mockmhr = start_patch('lae_automation.aws.queryapi.make_http_request')
        self.mockmhr.side_effect = self.mockmakehttprequestreturns

        self.mocktxawsS3Clientmakequeryfactory = start_patch('lae_automation.aws.devpay_s3client.DevPayS3Client._make_query_factory')

        self.mockrun_instances = start_patch('lae_automation.initialize.EC2Client.run_instances')
        self.mockrun_instances.return_value = defer.succeed([mock.Mock()])
        self.mockdescribe_instances = start_patch('lae_automation.initialize.EC2Client.describe_instances')
        self.mockdescribe_instances.return_value = defer.succeed( ('0.0.0.0', '0.0.0.0') )


    def tearDown(self):
        os.chdir(self.original_directory)
        [p.__exit__() for p in self._patchers]

    def runsideeffects(self, *args, **kwargs):
        if args[0] == 'whoami':
            return 'ubuntu'

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
        su_deferred = signup(mactivationkey, mproductcode, mname, memail, mkeyinfo, mstdout, mstderr, mseed, msecretsfile)
        return su_deferred    

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

