import os
from StringIO import StringIO

from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_util.fileutil import make_dirs
from lae_site.handlers import submit_subscription
from lae_util import send_email

MOCKAPIKEY = "sk_live_"+"A"*24
MOCKSMTPPASSWORD = "beef"*4 
REQUESTARGS = {'stripeToken':"STUB", 'email':"STUB2", 'pgp_pubkey':"STUB3"}
MOCKFURLFP = "FAKEPATH"

class MockRequest(object):
    def __init__(self, argdict):
        self.args = argdict

class MockFlappCommand(object):
    def __init__(self, furlfile):
        self.should_succeed = True
    def start(self):
        return defer.succeed(None)
    def run(self, content, stdout):
        return defer.succeed(None)

class MockPlan(object):
    def __init__(self):
        self.id = "MOCKS4"
        

class MockSubscription(object):
    def __init__(self):
        self.id = "sub_"+"A"*14
        self.plan = MockPlan()

class MockCustomer(object):
    def __init__(self):
        self.subscription = MockSubscription()
        self.email = 'EMAILSTUB'
        self.id = 'IDSTUB'

    @classmethod
    def create(cls, api_key, card, plan, email):
        return MockCustomer()

class MockCardError(Exception):
    def __init__(self, value):
        self.value = value

class MockAPIError(Exception):
    def __init__(self, value):
        self.value = value

class MockInvalidRequestError(Exception):
    def __init__(self, value):
        self.value = value

class MockTemplate(object):
    def __init__(self, args):
        self.args = args
    def render(self, characters):
        return "STUB"

class MockEnv(object):
    def __init__(self):
        pass
    def get_template(self, htmltemplate):
        return MockTemplate(repr(htmltemplate))
    
class TestSubmitSubscriptionHandler(TestCase):
    def setUp(self):
        #Patch out environment
        self.patch(submit_subscription, 'flappcommand', MockFlappCommand(MOCKFURLFP))
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer)
        self.patch(submit_subscription.stripe, 'CardError', MockCardError)
        self.patch(submit_subscription.stripe, 'APIError', MockAPIError)
        self.patch(submit_subscription.stripe, 'InvalidRequestError', MockInvalidRequestError)
        self.patch(submit_subscription, 'env', MockEnv())

        #Create directory for file IO
        temp = self.mktemp()
        self.basedirfp = FilePath(temp.rsplit('/',2)[0])
        os.rmdir(temp.rsplit('/',1)[0])

        #Create mock api key
        make_dirs(self.basedirfp.child('secret_config').path)
        self.basedirfp.child('secret_config').child('stripeapikey').setContent(MOCKAPIKEY)
        self.basedirfp.child('secret_config').child('smtppassword').setContent(MOCKSMTPPASSWORD)
        self.mocksmtppswdpath = self.basedirfp.child('secret_config').child('smtppassword').path
        self.patch(send_email, 'SMTP_PASSWORD_PATH', self.mocksmtppswdpath)
        

    def tearDown(self):
        self.basedirfp = ''

    def test_render(self):
        ssubhand_obj = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        mockrequest = MockRequest(REQUESTARGS)
        ssubhand_obj.render(mockrequest)

    def test_successful_subscription(self):
        ssubhand_obj = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
