from cStringIO import StringIO

from twisted.trial.unittest import TestCase
from twisted.internet import defer

import mock, sys
from lae_automation.signup import signup

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

    def tearDown(self):
        [p.__exit__() for p in self._patchers]
 
    def test_signup(self):
        #Patch out calls to make_http_request.  Keeps the test local, i.e. no need to communicate over-the-wire.
        from lae_automation.aws.queryapi import time
        def call_time():
            return 0
        self.patch(time, 'time', call_time)

        from lae_automation.aws import queryapi
        def call_make_http_request(request_url):
            self.failUnlessEqual(request_url, self.fakeURLs.pop(0))
            return defer.succeed(self.mhr_return_values.pop(0))
        self.patch(queryapi, 'make_http_request', call_make_http_request)            

        #Because the S3 Client call to S3 is made through txaws, it circumvents make_http_request, and necessitates a seperate patch to isolate the system from remote components.  The patched function is the submit method of the query object in initialize.  This attribute belongs to the Query class object in:  
        from lae_automation.aws.devpay_s3client import Query
        def call_query_submit(QueryObject):
            header_dict = QueryObject.get_headers()
            self.failUnlessEqual(header_dict['Date'], 'Thu, 01 Jan 1970 00:00:00 GMT') 
            self.failUnlessEqual(header_dict['Content-Length'], 0)
            self.failUnlessEqual(header_dict['Authorization'], 'AWS TESTAAAAAAAAAAAAAAAA:NlnzOWOmMCut8/Opl26UpAAiIhE=')
            self.failUnlessEqual(header_dict['x-amz-security-token'],'{UserToken}TESTUSERTOKEN%s==,{ProductToken}TESTPRODUCTTOKEN%s='%('A'*385, 'A'*295))
            self.failUnlessEqual(header_dict['Content-MD5'], '1B2M2Y8AsgTpgAmY7PhCfg==')
            return defer.succeed('Completed devpay bucket creation submission.')
        self.patch(Query, 'submit', call_query_submit)

        from lae_automation.initialize import EC2Client
        def call_run_instances(EC2Client,
                               ami_image_id,
                               mininstancecount,
                               maxinstancecount,
                               secgroups,
                               keypair_name,
                               instance_type):                             
            self.failUnlessEqual(ami_image_id, 'ami-testfbc2')
            self.failUnlessEqual(mininstancecount, 1)
            self.failUnlessEqual(maxinstancecount, 1)
            self.failUnlessEqual(secgroups, ['CustomerDefault'])
            self.failUnlessEqual(keypair_name, 'EC2MOCKYKEYS2')
            class MOCKEC2:
                def __init__(self):
                    self.instance_id = 'MOCKEC2INSTANCEID'
            return defer.succeed([MOCKEC2()])
        self.patch(EC2Client, 'run_instances', call_run_instances)
        def call_describe_instances(EC2Client, instance_id):
            self.failUnlessEqual(instance_id, 'MOCKEC2INSTANCEID')
            return defer.succeed( ('0.0.0.0', '0.0.0.0') )
        self.patch(EC2Client, 'describe_instances', call_describe_instances)


        from lae_automation import signup as signup_servercontainer
        def call_install_server(public_host, key_filename, stdout, stderr):
            self.failUnlessEqual(public_host, '0.0.0.0')
            self.failUnlessEqual(key_filename, 'EC2MOCKKEYFILENAME.pem')
        self.patch(signup_servercontainer, 'install_server', call_install_server)        
        def call_bounce_server(public_host,
                               key_filename,
                               private_host,
                               creds,
                               user_token,
                               product_token,
                               bucket_name,
                               stdout,
                               stderr,
                               secretsfile):
            self.failUnlessEqual(public_host, '0.0.0.0')
            self.failUnlessEqual(key_filename, 'EC2MOCKKEYFILENAME.pem')
            self.failUnlessEqual(private_host, '0.0.0.0')
            self.failUnlessEqual(user_token, '{UserToken}TESTUSERTOKEN%s=='%('A'*385,))
            self.failUnlessEqual(product_token, '{ProductToken}TESTPRODUCTTOKEN%s='%('A'*295,))
            self.failUnlessEqual(bucket_name, 'lae-abcdefgh-MSEED')
            self.failUnlessEqual(secretsfile, 'MSECRETSFILE')
            self.failUnlessEqual(creds.access_key, 'TESTAAAAAAAAAAAAAAAA')
            self.failUnlessEqual(creds.secret_key, 'TESTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA')
        self.patch(signup_servercontainer, 'bounce_server', call_bounce_server)

        def call_send_signup_confirmation(customer_name, customer_email, furl, customer_keyinfo, stdout, stderr):
            self.failUnlessEqual(customer_name, 'MNAME')
            self.failUnlessEqual(customer_email, 'MEMAIL')
            self.failUnlessEqual(furl, None)
            self.failUnlessEqual(customer_keyinfo, 'MKEYINFO')
            return defer.succeed("Tested send confirmation email call!")

        self.patch(signup_servercontainer, 'send_signup_confirmation', call_send_signup_confirmation)

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

        su_deferred = signup(MACTIVATIONKEY, MPRODUCTCODE, MNAME, MEMAIL, MKEYINFO, MSTDOUT, MSTDERR, MSEED, MSECRETSFILE, CONFIGFILEPATH, EC2SECRETPATH)
        return su_deferred
