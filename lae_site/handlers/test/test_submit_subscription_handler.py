
from twisted.internet import defer
from twisted.trial.unittest import TestCase

from lae_util.testutil import TestUtilitiesMixin
from lae_util.fileutil import make_dirs
from lae_site.handlers import submit_subscription
from lae_util import send_email
from lae_site.handlers.submit_subscription import stripe
from lae_site.handlers.submit_subscription import SubmitSubscriptionHandler
from lae_site.handlers.submit_subscription import RenderErrorDetailsForBrowser

MOCKAPIKEY = "sk_live_"+"A"*24
MOCKSMTPPASSWORD = "beef"*4
REQUESTARGS = {'stripeToken':["MOCK_stripe_token"], 'email':["test@test"], 'pgp_pubkey':["testpgppubkey"]}
MOCKFURLFP = "FAKEPATH"

def _append(seq, x):
    seq.append(x)
    return x

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
    def __init__(self, definition_string):
        self.definition_string = definition_string
    def render(self, kwargs):
        return self.definition_string.format(**kwargs)


class MockEnv(object):
    def __init__(self):
        pass
    def get_template(self, htmltemplate):
        return MockTemplate(repr(htmltemplate))


class CommonFixture(TestCase, TestUtilitiesMixin):
    def setUp(self):
        # Create directory for file I/O.
        self.basedirfp = self.create_workdirfp()

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


class TestRenderWithoutExceptions(CommonFixture):
    def record_return_value(self, return_value):
        self.render_method_local_assignments.append(return_value)
        return return_value

    def setUp(self):
        super(TestRenderWithoutExceptions, self).setUp()

        # In render method assignment list, stores local variables for checking
        self.get_creation_parameters_return_values = []
        self.create_customers_return_values = []
        self.env_get_template_return_values = []
        self.template_render_return_values = []
        self.basefp_child_return_values = []
        self.append_record_return_values = []
        self.run_full_signup_return_values = []

        # Define mocks and patch out called functions
        def call_get_creation_parameters(submit_subscription_handler_instance, request):
            return _append(self.get_creation_parameters_return_values,
                           (MOCKAPIKEY,
                            request.args['stripeToken'][0],
                            request.args['email'][0]))
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'get_creation_parameters',
                   call_get_creation_parameters)

        def call_create_customer(submit_subscription_handler_instance, stripe_api_key,
                                 stripe_authorization_token, user_email):
            return _append(self.create_customers_return_values,
                           MockCustomer.create(MockCustomer(),
                                               stripe_api_key,
                                               'card', 's4',
                                               user_email))
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)

        def call_get_template(target_template):
            return _append(self.env_get_template_return_values,
                           MockTemplate("Test template:\n"+
                                        "productfullname: {productfullname}\n"+
                                        "productname: {productname}"))
        self.patch(submit_subscription.env, 'get_template', call_get_template)

        def call_basefp_child(submit_subscription_handler_instance, PATH_TO_SUBSCRIPTIONS_FILE):
            return_value = submit_subscription_handler_instance.basefp.child(PATH_TO_SUBSCRIPTIONS_FILE)
            return _append(self.basefp_child_return_values,
                           return_value)

        def call_append_record(log_file_path, customer_subscription_id):
            return _append(self.append_record_return_values,
                           ())
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


    def tearDown(self):
        super(TestRenderWithoutExceptions, self).tearDown()

    def test_get_creation_parameters_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.get_creation_parameters_return_values), 1)

    def test_create_customer_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.create_customers_return_values), 1)

    """
    def test_basefp_child_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.basefp_child_return_values), 1)
        #self.failUnlessEqual(len(self.env_get_template_return_values), 1)
    """

    def test_get_creation_parameters(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(self.get_creation_parameters_return_values[0],
                             (MOCKAPIKEY,
                              REQUESTARGS['stripeToken'][0],
                              REQUESTARGS['email'][0]))

    """
    def test_create_customer(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self), 2)
        self.failUnless(isinstance(self.render_method_local_assignments[1], MockCustomer),
                        self.render_method_local_assignments[1])
        self.failUnless(isinstance(self.render_method_local_assignments[1].subscription,
                                   MockSubscription),
                        self.render_method_local_assignments[1].subscription)
        self.failUnlessEqual(self.render_method_local_assignments[1].email,
                             REQUESTARGS['email'][0],
                             self.render_method_local_assignments[1].email)
        self.failUnlessEqual(self.render_method_local_assignments[1].id, 'IDSTUB',
                             self.render_method_local_assignments[1].id)
        self.failUnlessEqual(self.render_method_local_assignments[1].init_email,
                             REQUESTARGS['email'][0],
                             self.render_method_local_assignments[1].init_email)
        self.failUnlessEqual(self.render_method_local_assignments[1].init_plan,
                             's4',
                             self.render_method_local_assignments[1].init_plan)
   """

class TestRenderWithException(CommonFixture):
    def record_return_value(self, return_value):
        self.render_method_local_assignments.append(return_value)
        return return_value

    def setUp(self):
        super(TestRenderWithException, self).setUp()

        # In render method assignment list, stores local variables for checking
        self.get_creation_parameters_return_values = []
        self.create_customers_return_values = []
        self.env_get_template_return_values = []
        self.template_render_return_values = []
        self.basefp_child_return_values = []
        self.append_record_return_values = []
        self.run_full_signup_return_values = []

        # Define mocks and patch out called functions
        def call_get_creation_parameters(submit_subscription_handler_instance, request):
            return _append(self.get_creation_parameters_return_values,
                           (MOCKAPIKEY,
                            request.args['stripeToken'][0],
                            request.args['email'][0]))
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'get_creation_parameters',
                   call_get_creation_parameters)

        def call_create_customer(submit_subscription_handler_instance, stripe_api_key,
                                 stripe_authorization_token, user_email):
            return _append(self.create_customers_return_values,
                           MockCustomer.create(MockCustomer(),
                                               stripe_api_key,
                                               'card', 's4',
                                               user_email))
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)

        def call_get_template(target_template):
            return _append(self.env_get_template_return_values,
                           MockTemplate("Test template:\n"+
                                        "productfullname: {productfullname}\n"+
                                        "productname: {productname}"))
        self.patch(submit_subscription.env, 'get_template', call_get_template)

        def call_basefp_child(submit_subscription_handler_instance, PATH_TO_SUBSCRIPTIONS_FILE):
            return_value = submit_subscription_handler_instance.basefp.child(PATH_TO_SUBSCRIPTIONS_FILE)
            return _append(self.basefp_child_return_values,
                           return_value)

        def call_append_record(log_file_path, customer_subscription_id):
            return _append(self.append_record_return_values,
                           ())
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


    def tearDown(self):
        super(TestRenderWithException, self).tearDown()

    def test_get_creation_parameters_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.get_creation_parameters_return_values), 1)

    def test_create_customer_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.create_customers_return_values), 1)

    """
    def test_basefp_child_calls(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self.basefp_child_return_values), 1)
        #self.failUnlessEqual(len(self.env_get_template_return_values), 1)
    """

    def test_get_creation_parameters(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(self.get_creation_parameters_return_values[0],
                             (MOCKAPIKEY,
                              REQUESTARGS['stripeToken'][0],
                              REQUESTARGS['email'][0]))

    """
    def test_create_customer(self):
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnlessEqual(len(self), 2)
        self.failUnless(isinstance(self.render_method_local_assignments[1], MockCustomer),
                        self.render_method_local_assignments[1])
        self.failUnless(isinstance(self.render_method_local_assignments[1].subscription,
                                   MockSubscription),
                        self.render_method_local_assignments[1].subscription)
        self.failUnlessEqual(self.render_method_local_assignments[1].email,
                             REQUESTARGS['email'][0],
                             self.render_method_local_assignments[1].email)
        self.failUnlessEqual(self.render_method_local_assignments[1].id, 'IDSTUB',
                             self.render_method_local_assignments[1].id)
        self.failUnlessEqual(self.render_method_local_assignments[1].init_email,
                             REQUESTARGS['email'][0],
                             self.render_method_local_assignments[1].init_email)
        self.failUnlessEqual(self.render_method_local_assignments[1].init_plan,
                             's4',
                             self.render_method_local_assignments[1].init_plan)

    def test_exceptional_get_tmpl_and_tmpl_render(self):
        # Stripe service returns exception raising error message, AND
        # Handling of stripe generated exceptions succeeds.
        def call_create_customer(submit_subscription_handler_instance, stripe_api_key,
                                 stripe_authorization_token, user_email):
            raise RenderErrorDetailsForBrowser('Handling of stripe generated exceptions succeeds,'\
                                                   'without generating "local" exception.')
        self.patch(submit_subscription.SubmitSubscriptionHandler, 'create_customer',
                   call_create_customer)

        def call_get_template(target_template):
            return self.record_return_value(MockTemplate("Test template:\nerrorblock: {errorblock}"))
        self.patch(submit_subscription.env, 'get_template', call_get_template)

        # Call render
        self.subscription_handler = submit_subscription.SubmitSubscriptionHandler(self.basedirfp)
        self.subscription_handler.render(MockRequest(REQUESTARGS))

        self.failUnless(isinstance(self.render_method_local_assignments[1], MockTemplate),
                        self.render_method_local_assignments)
        self.failUnlessEqual(len(self.render_method_local_assignments), 2)
   """
