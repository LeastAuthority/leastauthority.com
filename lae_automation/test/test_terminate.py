
#import mock

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath
from twisted.internet import defer

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint

from lae_util.fileutil import make_dirs
from lae_util.namespace import Namespace
from lae_automation import terminate
from lae_automation.config import Config


class TestTerminate(TestCase):
    SECRETKEY = "supersecret"
    ACCESSKEYID = "accesskeyid"
    INSTANCEID = 'i-instance'
    EMAIL = 'noone@example.org'
    CUSTOMERID = 'cus_signup'

    def setUp(self):
        self.testdir = FilePath('./test_terminate').child('TestTerminate')
        make_dirs(self.testdir.path)

    def make_config(self):
        secretkeyfp = self.testdir.child('secret')
        secretkeyfp.setContent(self.SECRETKEY)
        testconfigfp = self.testdir.child('test_config.json')
        testconfigfp.setContent("""
{ "products": [], "ssec2_access_key_id": "%s", "ssec2_secret_path": "%s" }
"""
% (self.ACCESSKEYID, secretkeyfp.path))
        return Config(testconfigfp.path)

    def make_signupfp(self):
        secretsfp = self.testdir.child('secrets')
        signupfp = secretsfp.child('signup')
        make_dirs(signupfp.path)
        return signupfp

    def make_signup(self, signupfp):
        signupfp.child('stripe').setContent('["%s", "", "%s", "S4_blah", "sub_dummy"]'
                                            % (self.EMAIL, self.CUSTOMERID))

    def make_signup_logs(self, signupfp):
        signupfp.child('signup_logs').setContent("""
...
<txaws.ec2.model.Instance object at ...>
  instance_id = '%s'
...
""" % (self.INSTANCEID,))


    def test_find_customer_signup(self):
        secretsfp = self.testdir.child('secrets')
        dummyfp = secretsfp.child('dummy')
        make_dirs(dummyfp.path)

        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              self.EMAIL, self.CUSTOMERID, secretsfp)

        dummyfp.child('stripe').setContent('["nomatch", "", "cus_signup", "S4_blah", "sub_dummy"]')
        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              self.EMAIL, self.CUSTOMERID, secretsfp)

        dummyfp.child('stripe').setContent('["noone@example.org", "", "cus_nomatch", "S4_blah", "sub_dummy"]')
        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              self.EMAIL, self.CUSTOMERID, secretsfp)

        signupfp = self.make_signupfp()
        self.make_signup(signupfp)
        self.failUnlessEqual(terminate.find_customer_signup(self.EMAIL, self.CUSTOMERID, secretsfp),
                             signupfp)

    def test_find_instance_id_for_signup(self):
        signupfp = self.make_signupfp()
        self.failUnlessRaises(IOError, terminate.find_instance_id_for_signup, signupfp)

        self.make_signup_logs(signupfp)
        self.failUnlessEqual(terminate.find_instance_id_for_signup(signupfp),
                             self.INSTANCEID)

    def test_delete_ec2_instance(self):
        ns = Namespace()
        ns.called = False

        class FakeEC2Client(object):
            def __init__(EC2ClientObject, creds, endpoint):
                self.failUnlessIsInstance(creds, AWSCredentials)
                self.failUnlessIsInstance(endpoint, AWSServiceEndpoint)
            def terminate_instances(EC2ClientObject, *instanceids):
                self.failUnlessEqual(instanceids, (self.INSTANCEID,))
                ns.called = True
                return defer.succeed(None)

        self.patch(terminate, 'EC2Client', FakeEC2Client)

        config = self.make_config()
        terminate.delete_ec2_instance(config, self.INSTANCEID)
        self.failUnless(ns.called)

    def test_load_ec2_credentials(self):
        config = self.make_config()
        creds = terminate.load_ec2_credentials(config)
        self.failUnlessIsInstance(creds, AWSCredentials)
        self.failUnlessEqual(creds.access_key, self.ACCESSKEYID)
        self.failUnlessEqual(creds.secret_key, self.SECRETKEY)

    def test_terminate_customer_server(self):
        ns = Namespace()
        ns.called = False

        def call_delete_ec2_instance(config, instanceid):
            self.failUnlessIsInstance(config, Config)
            self.failUnlessIsInstance(instanceid, self.INSTANCEID)
            ns.called = True
            return defer.succeed(None)

        self.patch(terminate, 'delete_ec2_instance', call_delete_ec2_instance)

        signupfp = self.make_signupfp()
        self.make_signup(signupfp)
        self.make_signup_logs(signupfp)
        terminate.terminate_customer_server(self.EMAIL, self.CUSTOMERID, self.testdir.child('secrets'))
        self.failUnless(ns.called)
