
#import mock

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath
from twisted.internet import defer

from lae_util.fileutil import make_dirs
from lae_util.namespace import Namespace
from lae_automation import terminate


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

        from lae_automation.initialize import EC2Client
        def call_terminate_instances(EC2ClientObject, *instanceids):
            self.failUnlessEqual(instanceids, ['i-instance'])
            ns.called = True
            return defer.succeed(None)

        self.patch(EC2Client, 'terminate_instances', call_terminate_instances)
        #...
        #self.failUnless(ns.called)

    def test_load_ec2_credentials(self):
        pass

    def test_terminate_customer_server(self):
        pass
