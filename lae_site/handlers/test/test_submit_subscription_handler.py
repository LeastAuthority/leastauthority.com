
from twisted.internet import defer
from twisted.trial.unittest import TestCase

from lae_util.testutil import TestUtilitiesMixin
from lae_util.fileutil import make_dirs
from lae_site.handlers import submit_subscription
from lae_util import send_email
from lae_site.handlers.submit_subscription import stripe
from lae_site.handlers.submit_subscription import SubmitSubscriptionHandler

MOCKAPIKEY = "sk_live_"+"A"*24
MOCKSMTPPASSWORD = "beef"*4
REQUESTARGS = {'stripeToken':["MOCK_stripe_token"], 'email':["test@test"], 'pgp_pubkey':["testpgppubkey"]}
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
    def render(self, section_dict):
        # XXX TODO make return value "realistic" function of section_dict
        return "FAKEVALUESTRING"


class MockEnv(object):
    def __init__(self):
        pass
    def get_template(self, htmltemplate):
        return MockTemplate(repr(htmltemplate))


class CommonFixture(TestCase, TestUtilitiesMixin):
    def setUp(self):
        # Create directory for file I/O.
        self.basedirfp = self.create_workdir()
        # There should never be a "real" customer instance
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer())
        # The Subcription Handler Instance
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)


class TestStripeErrorHandling(CommonFixture):
    def _test_stripe_error(self, MockErrorClass, expected_details_prefix, expected_subject):
        self.mc = MockCustomer()
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise MockErrorClass('THIS SHOULD BE THE VALUE STRIPE SENDS.')
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)

        calls = []
        def call_handle_stripe_create_customer_errors(submit_subscription_handler_obj, trace_back,
                                                      error, details, email_subject):
            calls.append((details, email_subject))
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors',
                   call_handle_stripe_create_customer_errors)

        self.subscription_handler.create_customer(MOCKAPIKEY, REQUESTARGS['stripeToken'][0],
                                                  REQUESTARGS['email'][0])

        # Check expectations.
        self.failUnlessEqual(len(calls), 1)
        (details, email_subject) = calls[0]
        self.failUnless(details.startswith(expected_details_prefix), details)
        self.failUnlessEquals(email_subject, expected_subject)

    # fixture code complete, actual tests follow
    def test_stripe_CardError(self):
        self.patch(stripe, 'CardError', MockCardError)
        return self._test_stripe_error(MockCardError, "Note: ", "Stripe Card error")

    def test_stripe_APIError(self):
        self.patch(stripe, 'APIError', MockAPIError)
        return self._test_stripe_error(MockAPIError, "Our ", "Stripe API error")

    def test_stripe_InvalidRequestError(self):
        self.patch(stripe, 'InvalidRequestError', MockInvalidRequestError)
        return self._test_stripe_error(MockInvalidRequestError, "Due ", "Stripe Invalid Request error")

    def test_stripe_UnexpectedError(self):
        return self._test_stripe_error(Exception, "Something ", "Stripe unexpected error")


class TestRender(CommonFixture):
    def setUp(self):
        super(TestRender, self).setUp()
        # Define mocks and patch out called functions
        def call_get_creation_parameters(submit_subscription_handler_instance, request):
            return MOCKAPIKEY, request.args['stripeToken'][0], request.args['email'][0]
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'get_creation_parameters',
                   call_get_creation_parameters)
        def call_create_customer(submit_subscription_handler_instance, stripe_api_key,
                                 stripe_authorization_token, user_email):
            return MockCustomer.create(MockCustomer(), stripe_api_key, 'card', 's4', user_email)
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)
        def call_env_dot_get_template(template_name):
            return MockTemplate(template_name)
        self.patch(submit_subscription.env, 'get_template',
                   call_env_dot_get_template)
        def call_append_record(log_file_path, customer_subscription_id):
            pass # XXX TODO make this more interesting
        self.patch(submit_subscription, 'append_record',
                   call_append_record)
        def call_run_full_signup(submit_subscription_handler_instance, customer, request):
            pass # XXX TODO make this more interesting
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'run_full_signup',
                   call_run_full_signup)

        # Create mock API key.
        make_dirs(self.basedirfp.child('secret_config').path)
        self.basedirfp.child('secret_config').child('stripeapikey').setContent(MOCKAPIKEY)
        self.basedirfp.child('secret_config').child('smtppassword').setContent(MOCKSMTPPASSWORD)
        self.mock_smtp_password_path = self.basedirfp.child('secret_config').child('smtppassword').path
        self.patch(send_email, 'SMTP_PASSWORD_PATH', self.mock_smtp_password_path)

        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)

    def tearDown(self):
        super(TestRender, self).tearDown()

    def test_stripe_successful_customer_creation(self):
        mockrequest = MockRequest(REQUESTARGS)
        self.subscription_handler.render(mockrequest)

        # self.failUnless(isinstance(self.mc, MockCustomer))
        # self.failUnlessEqual(self.mc.init_email, 'test@test')
        # self.failUnlessEqual(self.mc.email, 'test@test')
        # self.failUnlessEqual(self.mc.init_plan, 'S4')
