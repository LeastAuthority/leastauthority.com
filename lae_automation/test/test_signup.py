
from cStringIO import StringIO
from twisted.trial.unittest import TestCase
from twisted.internet import defer
from twisted.python.filepath import FilePath

from lae_automation import signup


# Vector data for request responses: activate desktop-, verify-, and describeEC2- responses.
USERTOKEN = 'TESTUSERTOKEN'+'A'*385
ACCESSKEYID = 'TEST'+'A'*16
SECRETACCESSKEY = 'TEST'+'A'*36
REQUESTID = 'TEST'+'A'*32
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295

# Test vector requests and responses to the different make http requests: activation, verification, describe instances
# ActivateDesktopProduct
adprequestresponse = """<ActivateDesktopProductResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/">
  <ActivateDesktopProductResult>
    <UserToken>{UserToken}%s==</UserToken>
    <AWSAccessKeyId>%s</AWSAccessKeyId>
    <SecretAccessKey>%s</SecretAccessKey>
  </ActivateDesktopProductResult>
  <ResponseMetadata>
    <RequestId>%s</RequestId>
  </ResponseMetadata>
</ActivateDesktopProductResponse>""" % (USERTOKEN, ACCESSKEYID, SECRETACCESSKEY, REQUESTID)

adphttprequestheader = """https://ls.amazonaws.com/?Action=ActivateDesktopProduct&ActivationKey=MOCKACTIVATONKEY&ProductToken=%7BProductToken%7DTESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D&Version=2008-04-28"""

# VerifyProductSubscriptionByTokens
verifyhttprequestheader = """https://ls.amazonaws.com/?Action=VerifyProductSubscriptionByTokens&AWSAccessKeyId=TESTAAAAAAAAAAAAAAAA&Expires=1970-01-01T00%3A15%3A00Z&ProductToken=%7BProductToken%7DTESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D&SignatureVersion=1&UserToken=%7BUserToken%7DTESTUSERTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D%3D&Version=2008-04-28&Signature=EoWZTlMO9qQA6Pbq5Ze4eHAlKZc%3D"""
verifyrequestresponse = """<VerifyProductSubscriptionByTokensResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/">
  <VerifyProductSubscriptionByTokensResult>
    <Subscribed>true</Subscribed>
  </VerifyProductSubscriptionByTokensResult>
  <ResponseMetadata>
    <RequestId>bd9db94b-a1b0-4a5f-8d70-6cc4de427623</RequestId>
  </ResponseMetadata>
</VerifyProductSubscriptionByTokensResponse>"""

# DescribeInstances
describeEC2instresponse = """<?xml version="1.0" encoding="UTF-8"?>
<DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/">
  <requestId>TEST</requestId>
  <reservationSet>
    <item>
      <reservationId>TEST</reservationId>
      <ownerId>TEST</ownerId>
      <groupSet><item><groupId>CustomerDefault</groupId></item></groupSet>
      <instancesSet>
        <item>
          <instanceId>TEST</instanceId>
          <imageId>TEST</imageId>
          <instanceState><code>TEST</code><name>TEST</name></instanceState>
          <privateDnsName>TESTinternal</privateDnsName>
          <dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName>
          <reason/>
          <keyName>TEST</keyName>
          <amiLaunchIndex>0</amiLaunchIndex>
          <productCodes/>
          <instanceType>t1.TEST</instanceType>
          <launchTime>TEST</launchTime>
          <placement><availabilityZone>TEST</availabilityZone></placement>
          <kernelId>TEST</kernelId>
        </item>
      </instancesSet>
    </item>
  </reservationSet>
</DescribeInstancesResponse>"""

# CreateTags
createtagsresponse = """<CreateTagsResponse xmlns="http://ec2.amazonaws.com/doc/2011-11-01/">
  <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
  <return>true</return>
</CreateTagsResponse>"""

# Vector data for the config file data:
CONFIGFILEJSON = """{
  "products": [
    { "full_name":     "The test vector product.",
      "product_code":  "ABCDEFGH",
      "product_token": "{ProductToken}TESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
      "ami_image_id":  "ami-testfbc2",
      "instance_size": "t1.testy"
    }
  ],
  "ec2_access_key_id": "TESTAAAAAAAAAAAAAAAA",
  "keypair_name":      "EC2MOCKYKEYS2",
  "key_filename":      "EC2MOCKKEYFILENAME.pem"
}"""

ZEROPRODUCT = """{
  "products": [],
  "ec2_access_key_id": "TESTAAAAAAAAAAAAAAAA",
  "keypair_name":      "EC2MOCKYKEYS2",
  "key_filename":      "EC2MOCKKEYFILENAME.pem"
}"""

MOCKEC2SECRETCONTENTS = 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'


class TestSignupModule(TestCase):
    def setUp(self):
        self.fakeURLs = [adphttprequestheader, verifyhttprequestheader]
        self.mhr_return_values = [adprequestresponse, verifyrequestresponse, describeEC2instresponse]
        self.CONFIGFILEPATH = 'init_test_config.json'
        self.EC2SECRETPATH = 'mock_ec2secret'

        FilePath(self.CONFIGFILEPATH).setContent(CONFIGFILEJSON)
        FilePath(self.EC2SECRETPATH).setContent(MOCKEC2SECRETCONTENTS)

        self.patch(signup, 'POLL_TIME', 0.1)
        self.patch(signup, 'CC_VERIFICATION_TIME', 0.3)
        self.patch(signup, 'ADDRESS_DELAY_TIME', 0.1)
        self.patch(signup, 'ADDRESS_WAIT_TIME', 0.3)
        self.patch(signup, 'LISTEN_POLL_TIME', 0.1)

        from lae_automation.aws.queryapi import time
        def call_time():
            return 0
        self.patch(time, 'time', call_time)

        from lae_automation.aws import queryapi
        def call_make_http_request(request_url):
            self.failUnlessEqual(request_url, self.fakeURLs.pop(0))
            return defer.succeed(self.mhr_return_values.pop(0))
        self.patch(queryapi, 'make_http_request', call_make_http_request)

        # Because the S3 Client call to S3 is made through txaws, it circumvents make_http_request,
        # and necessitates a separate patch to isolate the system from remote components.
        # The patched function is the submit method of the query object in initialize.
        # This attribute belongs to the Query class object imported by devpay_s3client.
        from lae_automation.aws.devpay_s3client import Query as S3_Query
        def call_s3_query_submit(QueryObject):
            header_dict = QueryObject.get_headers()
            self.failUnlessEqual(header_dict['Date'], 'Thu, 01 Jan 1970 00:00:00 GMT')
            self.failUnlessEqual(header_dict['Content-Length'], 0)
            self.failUnlessEqual(header_dict['Authorization'],
                                 'AWS TESTAAAAAAAAAAAAAAAA:NlnzOWOmMCut8/Opl26UpAAiIhE=')
            self.failUnlessEqual(header_dict['x-amz-security-token'],
                                 '{UserToken}TESTUSERTOKEN%s==,{ProductToken}TESTPRODUCTTOKEN%s=' % ('A'*385, 'A'*295))
            self.failUnlessEqual(header_dict['Content-MD5'], '1B2M2Y8AsgTpgAmY7PhCfg==')
            return defer.succeed('Completed devpay bucket creation submission.')
        self.patch(S3_Query, 'submit', call_s3_query_submit)

        from lae_automation.initialize import EC2Client
        def call_run_instances(EC2ClientObject, ami_image_id, mininstancecount, maxinstancecount,
                               secgroups, keypair_name, instance_type):
            self.failUnlessEqual(ami_image_id, 'ami-testfbc2')
            self.failUnlessEqual(mininstancecount, 1)
            self.failUnlessEqual(maxinstancecount, 1)
            self.failUnlessEqual(secgroups, ['CustomerDefault'])
            self.failUnlessEqual(keypair_name, 'EC2MOCKYKEYS2')
            class MockEC2Instance:
                def __init__(self):
                    self.instance_id = 'i-MOCKEC2INSTANCEID'
            return defer.succeed([MockEC2Instance()])
        self.patch(EC2Client, 'run_instances', call_run_instances)

        def call_describe_instances(EC2ClientObject, instance_id):
            self.failUnlessEqual(instance_id, 'i-MOCKEC2INSTANCEID')
            return defer.succeed( ('0.0.0.0', '0.0.0.1') )
        self.patch(EC2Client, 'describe_instances', call_describe_instances)

        from lae_automation.server import NotListeningError
        self.first = True
        def call_install_server(public_host, key_filename, stdout, stderr):
            self.failUnlessEqual(public_host, '0.0.0.0')
            self.failUnlessEqual(key_filename, 'EC2MOCKKEYFILENAME.pem')
            if self.first:
                self.first = False
                raise NotListeningError()
        self.patch(signup, 'install_server', call_install_server)

        def call_bounce_server(public_host, key_filename, private_host, creds, user_token, product_token,
                               bucket_name, stdout, stderr, secretsfile):
            self.failUnlessEqual(public_host, '0.0.0.0')
            self.failUnlessEqual(key_filename, 'EC2MOCKKEYFILENAME.pem')
            self.failUnlessEqual(private_host, '0.0.0.1')
            self.failUnlessEqual(user_token, '{UserToken}TESTUSERTOKEN%s=='%('A'*385,))
            self.failUnlessEqual(product_token, '{ProductToken}TESTPRODUCTTOKEN%s='%('A'*295,))
            self.failUnlessEqual(bucket_name, 'lae-abcdefgh-MSEED')
            self.failUnlessEqual(secretsfile, 'MSECRETSFILE')
            self.failUnlessEqual(creds.access_key, 'TESTAAAAAAAAAAAAAAAA')
            self.failUnlessEqual(creds.secret_key, 'TESTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')
        self.patch(signup, 'bounce_server', call_bounce_server)

        def call_send_signup_confirmation(customer_name, customer_email, furl, customer_keyinfo,
                                          stdout, stderr):
            self.failUnlessEqual(customer_name, 'MNAME')
            self.failUnlessEqual(customer_email, 'MEMAIL')
            self.failUnlessEqual(furl, None)
            self.failUnlessEqual(customer_keyinfo, 'MKEYINFO')
            return defer.succeed("Tested send confirmation email call!")
        self.patch(signup, 'send_signup_confirmation', call_send_signup_confirmation)

        from txaws.ec2.client import Query as EC2_Query
        def call_ec2_query_submit(QueryObject):
            return defer.succeed(createtagsresponse)
        self.patch(EC2_Query, 'submit', call_ec2_query_submit)

    def tearDown(self):
        FilePath(self.CONFIGFILEPATH).remove()
        FilePath(self.EC2SECRETPATH).remove()


    def test_signup(self):
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        stdout = StringIO()
        stderr = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'

        d = signup.signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, stdout, stderr,
                          MSEED, MSECRETSFILE, self.CONFIGFILEPATH, self.EC2SECRETPATH)
        return d

    def test_no_products(self):
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        stdout = StringIO()
        stderr = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'
        FilePath(self.CONFIGFILEPATH).setContent(ZEROPRODUCT)

        self.failUnlessRaises(AssertionError, signup.signup,
                              MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, stdout, stderr,
                              MSEED, MSECRETSFILE, self.CONFIGFILEPATH, self.EC2SECRETPATH)

    def test_timeout_verify(self):
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        stdout = StringIO()
        stderr = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'

        def call_verify_user_account(creds, usertoken, producttoken, stdout, stderr):
            return defer.succeed(False)
        self.patch(signup, 'verify_user_account', call_verify_user_account)

        d = signup.signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, stdout, stderr,
                          MSEED, MSECRETSFILE, self.CONFIGFILEPATH, self.EC2SECRETPATH)
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(signup.TimeoutError)
            out = stdout.getvalue()
            self.failUnlessIn("Timed out", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_timeout_addressreq(self):
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        stdout = StringIO()
        stderr = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'

        def call_get_EC2_addresses(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, instance_id):
            return defer.succeed(None)
        self.patch(signup, 'get_EC2_addresses', call_get_EC2_addresses)

        d = signup.signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, stdout, stderr,
                          MSEED, MSECRETSFILE, self.CONFIGFILEPATH, self.EC2SECRETPATH)
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(signup.TimeoutError)
            out = stdout.getvalue()
            self.failUnlessIn("Timed out", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_EC2_not_listening(self):
        MACTIVATIONKEY = 'MOCKACTIVATONKEY'
        MPRODUCTCODE = 'ABCDEFGH'
        MNAME = 'MNAME'
        MEMAIL = 'MEMAIL'
        MKEYINFO = 'MKEYINFO'
        stdout = StringIO()
        stderr = StringIO()
        MSEED = 'MSEED'
        MSECRETSFILE = 'MSECRETSFILE'

        from lae_automation.server import NotListeningError
        def call_install_server(public_host, key_filename, stdout, stderr):
            raise NotListeningError()
        self.patch(signup, 'install_server', call_install_server)

        d = signup.signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, stdout, stderr,
                          MSEED, MSECRETSFILE, self.CONFIGFILEPATH, self.EC2SECRETPATH)
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(signup.TimeoutError)
            out = stdout.getvalue()
            self.failUnlessIn("Timed out", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d
