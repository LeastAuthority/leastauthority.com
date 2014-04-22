
import simplejson
from twisted.trial.unittest import TestCase
from twisted.internet import defer

from lae_site.handlers import submit_subscription
from lae_site.handlers.submit_subscription import stripe
from lae_site.handlers.submit_subscription import SubmitSubscriptionHandler
from lae_site.handlers.submit_subscription import RenderErrorDetailsForBrowser

MOCKAPIKEY = "sk_live_"+"A"*24
MOCKSMTPPASSWORD = "beef"*4
MOCK_STRIPE_TOKEN = "MOCK_stripe_token"
MOCK_EMAIL = "test@test"
MOCK_PGP_PUBKEY = "testpgppubkey"
REQUESTARGS = {'stripeToken':[MOCK_STRIPE_TOKEN],
               'email':[MOCK_EMAIL],
               'pgp_pubkey':[MOCK_PGP_PUBKEY]}
MOCKFURLFP = "FAKEPATH"

def _append(seq, x):
    seq.append(x)
    return x


class MockFlappCommand(object):
    def __init__(self, fixture):
        self.fixture = fixture
    def run(self, input_json, log_file):
        d = defer.Deferred()
        self.fixture.flappcommand_run_return_values.append(d)
        return d


class MockCard(object):
    def __init__(self):
        self.value = 'MOCKCARD'


class MockRequest(object):
    def __init__(self, argdict):
        self.args = argdict


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
    def __init__(self, fixture, definition_string):
        self.fixture = fixture
        self.definition_string = definition_string
    def render(self, kwargs):
        rendered_string = self.definition_string.format(**kwargs)
        self.fixture.template_render_return_values.append(rendered_string)
        return rendered_string


class MockFilePath(object):
    def __init__(self, fixture, path):
        self.fixture = fixture
        self.fixture.FilePath_return_values.append(self)
        self.path = path

    def child(self, subpath):
        temp_path = "%s/%s" % (self.path, subpath)
        return MockFilePath(self.fixture, temp_path)

    def getContent(self):
        self.fixture.gotContent.append(MOCKAPIKEY)
        return MOCKAPIKEY

class MockOut(object):
    def __init__(self, fixture, name):
        fixture.mockoutput = self
        self.name = name
        self.values = []
    def write(self, value):
        self.values.append(value)


class MockAssertionError(object):
    pass


class CommonFixture(TestCase):
    def setUp(self):
        # Currently this is simply the stripe_api_key
        self.gotContent = []

        # Create directory for file I/O.
        self.FilePath_return_values = []
        self.basedirfp = MockFilePath(self, "MOCKWORKDIR")

        # There should never be a "real" customer instance
        self.patch(submit_subscription.stripe, 'Customer', MockCustomer())

        # Patch get_arg
        self.get_arg_return_values = []
        def call_get_arg(subscription_handler, request, argument):
            return _append(self.get_arg_return_values, request.args[argument])
        self.patch(SubmitSubscriptionHandler, 'get_arg', call_get_arg)

        # Patch send_plain_email
        self.send_plain_email_return_values = []
        def call_send_plain_email(from_address, to_address, tb, headers):
            self.failUnlessEqual(from_address, 'info@leastauthority.com')
            return _append(self.send_plain_email_return_values, defer.Deferred())
        self.patch(submit_subscription, 'send_plain_email', call_send_plain_email)

        # Patch append_record
        self.append_record_return_values = []
        def call_append_record(mock_log_file_path, customer_subscription_id):
            return _append(self.append_record_return_values, None)
        self.patch(submit_subscription, 'append_record', call_append_record)

        # The Subcription Handler Instance
        self.subscription_handler = SubmitSubscriptionHandler(self.basedirfp)


# Begin test of SubmitSubscriptionHandler.get_stripe_api_key
class TestGetStripeAPIKeyWithoutException(CommonFixture):
    def setUp(self):
        super(TestGetStripeAPIKeyWithoutException, self).setUp()
        self.subscription_handler.get_stripe_api_key()

    # Fake of FilePath handled by MockFilePath
    def test_correct_file_path_defined(self):
        self.failUnlessEqual(self.gotContent, [MOCKAPIKEY])


# Begin test of SubmitSubscriptionHandler.get_stripe_api_key
class TestGetStripeAPIKeyWithException(CommonFixture):
    def setUp(self):
        super(TestGetStripeAPIKeyWithException, self).setUp()
        self.subscription_handler.basefp.path = self.subscription_handler.basefp.path.replace(
            'MOCKWORKDIR','leastauthority.com')
        try:
            self.subscription_handler.get_stripe_api_key()
        except AssertionError, e:
            self.failUnlessEqual(e.message, "Secrets are not allowed in the production code repo:"+
                                 " 'leastauthority.com/secret_config/stripeapikey'")
    # Fake of FilePath handled by MockFilePath
    def test_correct_file_path_defined(self):
        self.failUnlessEqual(self.FilePath_return_values[0].path, 'leastauthority.com')
        self.failUnlessEqual(self.gotContent, [])


# Begin test of SubmitSubscriptionHandler.get_creation_parameters
class TestGetCreationParameters(CommonFixture):
    def setUp(self):
        super(TestGetCreationParameters, self).setUp()

        # Patch get_stripe_api_key
        self.get_stripe_api_key_return_values = []
        def call_get_stripe_api_key(subscription_handler):
            return _append(self.get_stripe_api_key_return_values, MOCKAPIKEY)
        self.patch(SubmitSubscriptionHandler, 'get_stripe_api_key', call_get_stripe_api_key)

        self.subscription_handler.get_creation_parameters(MockRequest(REQUESTARGS))

    def test_get_stripe_api_key_call(self):
        self.failUnlessEqual(self.get_stripe_api_key_return_values, [MOCKAPIKEY])

    def test_get_arg_calls(self):
        self.failUnlessEqual(self.get_arg_return_values, [['MOCK_stripe_token'], ['test@test']])

# Begin test of SubmitSubscriptionHandler.handle_stripe_create_customer_errors
class TestHandleStripeCreateCustomerErrors(CommonFixture):
    def setUp(self):
        super(TestHandleStripeCreateCustomerErrors, self).setUp()

        # Patch self.out
        self.patch(self.subscription_handler, 'out', MockOut(self, 'out'))

    def test_RenderErrorDetails_raise(self):
        try:
            self.subscription_handler.handle_stripe_create_customer_errors(\
                'test trace_back',
                MockCardError('test of handle_stripe_create_customer_errors'),
                'test details', 'test subject')
        except Exception, e:
            self.failUnless(isinstance(e, RenderErrorDetailsForBrowser))
            self.failUnlessEqual(e.details, 'test details')

    def test_send_plain_email(self):
        try:
            self.subscription_handler.handle_stripe_create_customer_errors(\
                'test trace_back',
                MockCardError('test of handle_stripe_create_customer_errors'),
                'test details', 'test subject')
        except:
            pass

    def test_print_outs(self):
        try:
            self.subscription_handler.handle_stripe_create_customer_errors(\
                'test trace_back',
                MockCardError('test of handle_stripe_create_customer_errors'),
                'test details', 'test subject')
        except:
            pass
        self.failUnless(isinstance(self.mockoutput, MockOut))
        self.failUnlessEqual(self.mockoutput.values,
                             ['Got MockCardError from the stripe.Customer.create call:',
                              '\n',
                              'test trace_back',
                              '\n'])


# Begin test of SubmitSubscriptionHandler.create_customer
class TestCreateCustomer(CommonFixture):
    def setUp(self):
        super(TestCreateCustomer, self).setUp()

    def _test_stripe_error(self, MockErrorClass, expected_details_prefix, expected_subject):
        self.mc = MockCustomer()
        def call_stripe_Customer_create(api_key, card, plan, email):
            raise MockErrorClass('THIS SHOULD BE THE VALUE STRIPE SENDS.')
        self.patch(stripe.Customer, 'create', call_stripe_Customer_create)

        calls = []
        def call_handle_stripe_create_customer_errors(subscription_handler, trace_back,
                                                      error, details, email_subject):
            calls.append((details, email_subject))
        self.patch(SubmitSubscriptionHandler, 'handle_stripe_create_customer_errors',
                   call_handle_stripe_create_customer_errors)

        self.subscription_handler.create_customer(MOCKAPIKEY, MOCK_STRIPE_TOKEN, MOCK_EMAIL)

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
        return self._test_stripe_error(MockInvalidRequestError, "Due ",
                                       "Stripe Invalid Request error")

    def test_stripe_UnexpectedError(self):
        return self._test_stripe_error(Exception, "Something ", "Stripe unexpected error")


# Begin test of SubmitSubscriptionHandler.run_full_signup
class TestRunFullSignup(CommonFixture):
    def setUp(self):
        super(TestRunFullSignup, self).setUp()
        self.MC = MockCustomer.create(MockCustomer(),
                                 MOCKAPIKEY,
                                 MockCard(),
                                 MockPlan(),
                                 MOCK_EMAIL)

        # Patch simplejson.dumps
        self.simplejson_dumps_returns_values = []
        def call_simplejson_dumps(arg_tuple, **ascii_enforce):
            return _append(self.simplejson_dumps_returns_values, arg_tuple)
        self.patch(simplejson, 'dumps', call_simplejson_dumps)

        self.flappcommand_run_return_values = []
        self.patch(submit_subscription, 'flappcommand', MockFlappCommand(self))
        self.subscription_handler.run_full_signup(self.MC, MockRequest(REQUESTARGS))


    def test_stdin_values(self):
        self.failUnlessEqual(self.simplejson_dumps_returns_values, [('test@test', ['testpgppubkey'],
                                                                     'IDSTUB', 'MOCKS4',
                                                                     'sub_AAAAAAAAAAAAAA')])

    def test_when_done(self):
        self.flappcommand_run_return_values[0].callback('ignore')
        self.failUnlessEqual(self.FilePath_return_values[1].path, 'MOCKWORKDIR/service_confirmed.csv')
        self.failUnlessEqual(self.append_record_return_values, [None])


# Begin test of SubmitSubscriptionHandler.render
class CommonRenderFixture(CommonFixture):
    def setUp(self):
        super(CommonRenderFixture, self).setUp()
        # In render method assignment list, stores local variables for checking
        self.get_creation_parameters_return_values = []
        self.create_customers_return_values = []
        self.env_get_template_return_values = []
        self.template_render_return_values = []
        self.run_full_signup_return_values = []

        def call_create_customer(): # Abstract
            pass

        def call_get_template(): # Abstract
            pass

        # Define mocks and patch out called functions
        def call_get_creation_parameters(subscription_handler, request):
            return _append(self.get_creation_parameters_return_values,
                           (MOCKAPIKEY,
                            request.args['stripeToken'][0],
                            request.args['email'][0]))
        self.patch(SubmitSubscriptionHandler, 'get_creation_parameters',
                   call_get_creation_parameters)

        # NOTE: mockery of calls to FilePath and FilePath.child handled by MockFilePath

        def call_run_full_signup(subscription_handler, customer, request):
            self.failUnless(isinstance(customer, MockCustomer), customer)
            self.failUnless(isinstance(request, MockRequest), request)
            return _append(self.run_full_signup_return_values, None)
        self.patch(SubmitSubscriptionHandler, 'run_full_signup',
                   call_run_full_signup)


class TestRenderWithoutExceptions(CommonRenderFixture):
    def setUp(self):
        super(TestRenderWithoutExceptions, self).setUp()

        def call_create_customer(subscription_handler, stripe_api_key,
                                 stripe_authorization_token, user_email):
            return _append(self.create_customers_return_values,
                           MockCustomer.create(MockCustomer(),
                                               stripe_api_key,
                                               'card', 's4',
                                               user_email))
        self.patch(SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)

        def call_get_template(target_template):
            self.mock_template = MockTemplate(self, "Test template:\n"+
                                        "productfullname: {productfullname}\n"+
                                        "productname: {productname}")
            return _append(self.env_get_template_return_values, self.mock_template)
        self.patch(submit_subscription.env, 'get_template', call_get_template)

        # Note render mockery is handled inside of MockTemplate

    def tearDown(self):
        super(TestRenderWithoutExceptions, self).tearDown()

    def test_get_creation_parameters_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.get_creation_parameters_return_values,
                             [(MOCKAPIKEY, MOCK_STRIPE_TOKEN, MOCK_EMAIL)])

    def test_create_customer_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.create_customers_return_values), 1)
        self.failUnless(isinstance(self.create_customers_return_values[0], MockCustomer),
                        self.create_customers_return_values[0])

    def test_basefp_child_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.FilePath_return_values[1].path, 'MOCKWORKDIR/subscriptions.csv')

    def test_append_record_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.append_record_return_values, [None])

    def test_run_full_signup_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.append_record_return_values, [None])

    def test_env_get_template_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(len(self.env_get_template_return_values), 1)
        self.failUnless(isinstance(self.env_get_template_return_values[0], MockTemplate),
                        self.env_get_template_return_values[0])

    def test_env_get_template(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(len(self.env_get_template_return_values), 1)
        self.failUnless(isinstance(self.env_get_template_return_values[0], MockTemplate),
                        self.env_get_template_return_values[0])

    def test_template_render(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.template_render_return_values,
                             ["Test template:\n"+
                              "productfullname: Simple Secure Storage Service\n"+
                              "productname: S4"])


class TestRenderWithExceptions(CommonRenderFixture):
    def setUp(self):
        super(TestRenderWithExceptions, self).setUp()
        def call_create_customer(subscription_handler, stripe_api_key,
                                 stripe_authorization_token, user_email):
            self.create_customers_return_values.append(None)
            raise RenderErrorDetailsForBrowser, 'MOCKERROR'
        self.patch(SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)

        def call_get_template(target_template):
            self.mock_template = MockTemplate(self,"Test template:\n"+
                                              "errorblock: {errorblock}")
            return _append(self.env_get_template_return_values, self.mock_template)
        self.patch(submit_subscription.env, 'get_template', call_get_template)

    def tearDown(self):
        super(TestRenderWithExceptions, self).tearDown()

    def test_get_creation_parameters_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.get_creation_parameters_return_values,
                             [(MOCKAPIKEY, MOCK_STRIPE_TOKEN, MOCK_EMAIL)])

    def test_create_customer_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.create_customers_return_values, [None])

    def test_env_get_template_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(len(self.env_get_template_return_values), 1)
        self.failUnless(isinstance(self.env_get_template_return_values[0], MockTemplate),
                        self.env_get_template_return_values[0])

    def test_template_render(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.template_render_return_values,
                             ["Test template:\nerrorblock: MOCKERROR"])

    def test_basefp_child_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(len(self.FilePath_return_values), 1)

    def test_append_record_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.append_record_return_values, [])

    def test_run_full_signup_calls(self):
        self.subscription_handler.render(MockRequest(REQUESTARGS))
        self.failUnlessEqual(self.append_record_return_values, [])
