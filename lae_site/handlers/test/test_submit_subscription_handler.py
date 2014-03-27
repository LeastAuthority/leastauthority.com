import os, sys

from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_util.fileutil import make_dirs
from lae_site.handlers import submit_subscription
from lae_util import send_email
from lae_site.handlers.submit_subscription import stripe
from lae_site.handlers.submit_subscription import SubmitSubscriptionHandler

MOCKAPIKEY = "sk_live_"+"A"*24
MOCKSMTPPASSWORD = "beef"*4
REQUESTARGS = {'stripeToken':["tokenstub"], 'email':["test@test"], 'pgp_pubkey':["testpgppubkey"]}
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
    def create(self, api_key, card, plan, email):
        self.subscription = MockSubscription()
        self.email = email
        self.id = 'IDSTUB'
        self.init_key = api_key
        self.init_email = email
        self.init_plan = plan
        return self


class MockCardError(Exception):
    def __init__(self, value):
        self.value = value
        self.message = value


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


class TestStripeErrorMessageHandling(TestCase):
    def setUp(self):
        #Patch out environment
        self.patch(submit_subscription, 'flappcommand', MockFlappCommand(MOCKFURLFP))
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer())
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

        self.mc = 'SettingUp'
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)

    def tearDown(self):
        self.basedirfp = 'TornDown'
        self.mc = 'TornDown'
        self.subscription_handler = 'TornDown'


class TestSpecificErrorTypes(TestStripeErrorMessageHandling):
    def test_stripe_CardError(self):
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise MockCardError('THIS SHOULD BE THE VALUE STRIPE SENDS.')

        def call_handle_stripe_create_customer_errors(submit_subscription_handler_obj, trace_back, error, details,
                                        email_subject):
            self.failUnless(details.startswith('Note: '))
            self.failUnlessEquals(email_subject, "Stripe Card error")
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors', call_handle_stripe_create_customer_errors)
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)
        self.patch(stripe, 'CardError', MockCardError)
        self.subscription_handler.create_customer(MOCKAPIKEY, REQUESTARGS['stripeToken'][0],
                                                  REQUESTARGS['email'][0])

    def test_stripe_APIError(self):
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise MockAPIError('THIS SHOULD BE THE VALUE STRIPE SENDS.')

        def call_handle_stripe_create_customer_errors(submit_subscription_handler_obj, trace_back, error, details,
                                        email_subject):
            self.failUnless(details.startswith('Our '))
            self.failUnlessEquals(email_subject, "Stripe API error")
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors', call_handle_stripe_create_customer_errors)
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)
        self.patch(stripe, 'APIError', MockAPIError)
        self.subscription_handler.create_customer(MOCKAPIKEY, REQUESTARGS['stripeToken'][0],
                                                  REQUESTARGS['email'][0])

    def test_stripe_InvalidRequestError(self):
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise MockInvalidRequestError('THIS SHOULD BE THE VALUE STRIPE SENDS.')

        def call_handle_stripe_create_customer_errors(submit_subscription_handler_obj, trace_back, error, details,
                                        email_subject):
            self.failUnless(details.startswith('Due '))
            self.failUnlessEquals(email_subject, "Stripe Invalid Request error")
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors', call_handle_stripe_create_customer_errors)
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)
        self.patch(stripe, 'InvalidRequestError', MockInvalidRequestError)
        self.subscription_handler.create_customer(MOCKAPIKEY, REQUESTARGS['stripeToken'][0],
                                                  REQUESTARGS['email'][0])

    def test_stripe_UnexpectedError(self):
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise Exception('THIS SHOULD BE THE VALUE STRIPE SENDS.')

        def call_handle_stripe_create_customer_errors(submit_subscription_handler_obj, trace_back, error, details,
                                        email_subject):
            self.failUnless(details.startswith('Something '))
            self.failUnlessEquals(email_subject, "Stripe unexpected error")
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors', call_handle_stripe_create_customer_errors)
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)
        self.subscription_handler.create_customer(MOCKAPIKEY, REQUESTARGS['stripeToken'][0],
                                                  REQUESTARGS['email'][0])

class TestSubscribedCustomerCreation(TestCase):
    def setUp(self):
        #Patch out environment
        self.patch(submit_subscription, 'flappcommand', MockFlappCommand(MOCKFURLFP))
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer())
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

        self.mc = 'SettingUp'
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)

    def tearDown(self):
        self.basedirfp = 'TornDown'
        self.mc = 'TornDown'
        self.subscription_handler = 'TornDown'

    def test_successful_customer_creation(self):
        mockrequest = MockRequest(REQUESTARGS)
        def call_stripe_Customer_create(api_key, card, plan, email):
            self.mc = MockCustomer()
            MockCustomer.create(self.mc, api_key, card, plan, email)
            return self.mc

        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)
        self.subscription_handler.render(mockrequest)

        self.failUnless(isinstance(self.mc, MockCustomer))
        self.failUnlessEqual(self.mc.init_key, 'sk_live_AAAAAAAAAAAAAAAAAAAAAAAA')
        self.failUnlessEqual(self.mc.init_email, 'test@test')
        self.failUnlessEqual(self.mc.email, 'test@test')
        self.failUnlessEqual(self.mc.init_plan, 'S4')
