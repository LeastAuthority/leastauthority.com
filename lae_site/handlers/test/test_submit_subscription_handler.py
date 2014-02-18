import os, StringIO

from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_util.fileutil import make_dirs
from lae_site.handlers import submit_subscription

MOCKAPIKEY = "sk_live_"+"A"*24 #<-- actual test key

class MockRequest(object):
    def __init__(self, method, d, args):
        self.responsecode = None
        self.out = StringIO()
        self.method = method
        self.args = args
        self.d = d
    def setResponseCode(self, code):
        self.responsecode = code
    def write(self, s):
        self.out.write(s)
    def finish(self):
        self.d.callback(self.out.getvalue())

class MockFlappCommand(object):
    def __init__(self, furlfile):
        self.should_succeed = True
    def start(self):
        return defer.succeed(None)
    def run(self, content, stdout):
        print >>stdout, "Starting..."
        return defer.succeed(None)

class MockSubscription(object):
    def __init__(self):
        self.id = "sub_"+"A"*14

class MockCustomer(object):
    def __init__(self):
        self.subscription = MockSubscription()

    @classmethod
    def create(cls, args):
        return MockCustomer()

class MockCardError(Exception):
    def __init__(self, value):
        self.value = value

class MockEnv(object):
    def __init__(self):
        pass
    
class TestSubmitSubscriptionHandler(TestCase):
    def setUp(self):
        #Patch out environment
        self.patch(submit_subscription, 'FlappCommand', MockFlappCommand)
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer)
        self.patch(submit_subscription.stripe, 'CardError', MockCardError)
        self.patch(submit_subscription, 'env', MockEnv)

        #Create directory for file IO
        temp = self.mktemp()
        self.basedir = temp.rsplit('/',2)[0]
        os.rmdir(temp.rsplit('/',1)[0])

        #Create mock api key
        make_dirs(FilePath(self.basedir).child('secret_config').path)
        FilePath(self.basedir).child('secret_config').child('stripeapikey').setContent(MOCKAPIKEY)
        
    def tearDown(self):
        self.basedir = ''

    def test_foo(self):
        submit_subscription.stripe.Customer.create('fo')
