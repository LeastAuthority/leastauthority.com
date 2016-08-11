
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
    def setUp(self):
        self.testdir = FilePath('./test_terminate').child('TestTerminate')
        make_dirs(self.testdir.path)

    def test_find_customer_signup(self):
        secretsfp = self.testdir.child('secrets')
        dummyfp = secretsfp.child('dummy')
        make_dirs(dummyfp.path)

        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              'noone@example.org', 'cus_signup', secretsfp)

        dummyfp.child('stripe').setContent('["nomatch", "", "cus_signup", "S4_blah", "sub_dummy"]')
        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              'noone@example.org', 'cus_signup', secretsfp)

        dummyfp.child('stripe').setContent('["noone@example.org", "", "cus_nomatch", "S4_blah", "sub_dummy"]')
        self.failUnlessRaises(KeyError, terminate.find_customer_signup,
                              'noone@example.org', 'cus_signup', secretsfp)

        signupfp = secretsfp.child('signup')
        make_dirs(signupfp.path)
        signupfp.child('stripe').setContent('["noone@example.org", "", "cus_signup", "S4_blah", "sub_dummy"]')

        self.failUnlessEqual(terminate.find_customer_signup('noone@example.org', 'cus_signup', secretsfp),
                             signupfp)

    def test_find_instance_id_for_signup(self):
        secretsfp = self.testdir.child('secrets')
        signupfp = secretsfp.child('signup')
        make_dirs(signupfp.path)

        self.failUnlessRaises(IOError, terminate.find_instance_id_for_signup, signupfp)

        signupfp.child('signup_logs').setContent("""
...
<txaws.ec2.model.Instance object at ...>
  instance_id = 'i-instance'
...
""")
        self.failUnlessEqual(terminate.find_instance_id_for_signup(signupfp),
                             'i-instance')

    def test_delete_ec2_instance(self):
        ns = Namespace()
        ns.called = False
        expected_instanceid = 'i-instance'

        class FakeEC2Client(object):
            def __init__(EC2ClientObject, creds, endpoint):
                self.failUnlessIsInstance(creds, AWSCredentials)
                self.failUnlessIsInstance(endpoint, AWSServiceEndpoint)
            def terminate_instances(EC2ClientObject, *instanceids):
                self.failUnlessEqual(instanceids, (expected_instanceid,))
                ns.called = True
                return defer.succeed(None)

        self.patch(terminate, 'EC2Client', FakeEC2Client)

        FilePath('secret').setContent("supersecret")
        FilePath('test_config.json').setContent("""
{ "products": [], "ssec2_access_key_id": "accesskeyid", "ssec2_secret_path": "secret" }
""")
        config = Config('test_config.json')
        terminate.delete_ec2_instance(config, expected_instanceid)
        self.failUnless(ns.called)

    def test_load_ec2_credentials(self):
        pass

    def test_terminate_customer_server(self):
        pass
