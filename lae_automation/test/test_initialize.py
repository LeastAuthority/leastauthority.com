
from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, os, sys
print mock

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

        self.describeEC2instresponse = """<DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/"><requestId>2f48cada-b92d-4f1d-98da-789c44eec9a4</requestId><reservationSet><item><reservationId>r-0453466a</reservationId><ownerId>459586236352</ownerId><groupSet><item><groupId>CustomerDefault</groupId></item></groupSet><instancesSet><item><instanceId>i-b14d07d2</instanceId><imageId>ami-ab36fbc2</imageId><instanceState><code>16</code><name>running</name></instanceState><privateDnsName>domU-12-31-38-07-19-09.compute-1.internal</privateDnsName><dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName><reason/><keyName>EC2adminkeys2</keyName><amiLaunchIndex>0</amiLaunchIndex><productCodes/><instanceType>t1.micro</instanceType><launchTime>2011-11-24T15:31:46.000Z</launchTime><placement><availabilityZone>us-east-1d</availabilityZone></placement><kernelId>aki-407d9529</kernelId></item></instancesSet></item></reservationSet></DescribeInstancesResponse>""" 
        self.return_values = [self.adprequestresponse, self.verifyrequestresponse, self.describeEC2instresponse]

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
        self.mockmhr.side_effect = self.mockmakehttprequestreturns

        self.mocktxawsS3Clientmakequeryfactory = start_patch('lae_automation.aws.devpay_s3client.DevPayS3Client._make_query_factory')
        #self.mocktxawsQuery = start_patch('lae_automation.aws.devpay_s3client.Query')
        self.mocktxawsEC2ClientRI = start_patch('lae_automation.initialize.EC2Client.run_instances')
        #self.mocktxawsEC2Parser = start_patch('lae_automation.initialize.txaws_ec2_Parser')
        self.mocktxawsEC2ClientRI.side_effect = self.mockEC2clientruninstances

    def tearDown(self):
        os.chdir(self.original_directory)
        [p.__exit__() for p in self._patchers]


    def mockmakehttprequestreturns(self, argument_to_make_http_request):
        return defer.succeed(self.return_values.pop(0))

    def mockEC2clientruninstances(self, *args, **kwargs):
        #print "This function 'mockEC2clientruninstances' was called with args: %s"%(args)
        return defer.succeed([mock.Mock()])

    def test_signup(self):
        # Arguments to signup
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

