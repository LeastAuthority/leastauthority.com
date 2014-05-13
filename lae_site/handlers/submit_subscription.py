
import stripe, traceback, simplejson

from lae_util.servers import append_record
from lae_util.send_email import send_plain_email, FROM_ADDRESS

from lae_site.handlers.web import env
from lae_util.flapp import FlappCommand

from lae_site.handlers.main import HandlerBase

SUBSCRIPTIONS_FILE      = 'subscriptions.csv'
SERVICE_CONFIRMED_FILE  = 'service_confirmed.csv'
SIGNUP_FURL_FILE        = 'signup.furl'

flappcommand = None

def start(basefp):
    global flappcommand

    signup_furl_fp = basefp.child('secret_config').child(SIGNUP_FURL_FILE)
    flappcommand = FlappCommand(signup_furl_fp.path)
    return flappcommand.start()

class RenderErrorDetailsForBrowser(Exception):
    def __init__(self, details):
        self.details = details


class SubmitSubscriptionHandler(HandlerBase):
    def __init__(self, basefp):
        HandlerBase.__init__(self, out=None)
        self._logger_helper(__name__)
        self.basefp = basefp

    # The following helper methods are all called directly or indirectly by render.
    def get_stripe_api_key(self):
        stripefp = self.basefp.child('secret_config').child('stripeapikey')
        if ('leastauthority.com' in stripefp.path) and ('_trial_temp' not in stripefp.path):
            raise AssertionError("Secrets are not allowed in the production code repo: %r" %
                                 (stripefp.path,))
        return stripefp.getContent().strip()

    def get_creation_parameters(self, request):
        # Load apikey.
        stripe_api_key = self.get_stripe_api_key()
        # Parse request, info from stripe and subscriber.
        stripe_authorization_token = self.get_arg(request, 'stripeToken')
        user_email = self.get_arg(request, 'email')
        return stripe_api_key, stripe_authorization_token, user_email

    def handle_stripe_create_customer_errors(self, trace_back, error, details, email_subject,
                                             notes=None):
        print >>self.out, "Got %s from the stripe.Customer.create call:" % (error.__class__.__name__,)
        print >>self.out, trace_back
        headers = {
            "From": FROM_ADDRESS,
            "Subject": email_subject,
        }
        if notes:
            combination = "%s\n%s" % (notes, trace_back)
            body = combination
        else:
            body = trace_back
        send_plain_email('info@leastauthority.com', 'support@leastauthority.com', body, headers)
        raise RenderErrorDetailsForBrowser(details)

    def create_customer(self, stripe_api_key, stripe_authorization_token, user_email):
        try:
            return stripe.Customer.create(api_key=stripe_api_key, card=stripe_authorization_token, plan='S4',
                                          email=user_email)
        except stripe.CardError as e:
            # Errors we expect: https://stripe.com/docs/api#errors
            note = "Note: This error could be caused by insufficient funds, or other charge-disabling "+\
                "factors related to the User's payment credential.\n"
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
                                                      details=e.message,
                                                      email_subject="Stripe Card error",
                                                      notes=note)
        except stripe.APIError as e:
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
                                        details="Our payment processor is temporarily unavailable,"+
                                            " please try again in\ a few moments.",
                                        email_subject="Stripe API error")
        except stripe.InvalidRequestError as e:
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
                                        details="Due to technical difficulties unrelated to your card"+
                                            " details, we were unable to charge your account. Our"+
                                            " engineers have been notified and will contact you with"+
                                            " an update shortly.",
                                        email_subject="Stripe Invalid Request error")
        except Exception as e:
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
                                        details="Something went wrong. Please try again, or contact"+
                                            " <support@leastauthority.com>.",
                                        email_subject="Stripe unexpected error")

    def run_full_signup(self, customer, request):
        def when_done(ign):
            service_confirmed_fp = self.basefp.child(SERVICE_CONFIRMED_FILE)
            try:
                append_record(service_confirmed_fp, customer.subscription.id)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error
                # locally.
                traceback.print_tb(100, self._log)

        def when_failed(ign):
            try:
                headers = {
                    "From": FROM_ADDRESS,
                    "Subject": "Sign-up error",
                }
                send_plain_email('info@leastauthority.com', 'support@leastauthority.com',
                                 "A sign-up failed for <%s>." % (customer.email,), headers)
            except Exception:
                traceback.print_tb(100, self._log)

        customer_pgpinfo = self.get_arg(request, 'pgp_pubkey')
        # The foolscap service registered to run when flappcommand.run is called expects a bytestream
        # of US-ASCII bytes, because it is reading from its stdin (--accept-stdin flag set).
        # Therefore the content passed to the command must conform to US-ASCII.
        stdin = simplejson.dumps((customer.email,
                                  customer_pgpinfo,
                                  customer.id,
                                  customer.subscription.plan.id,
                                  customer.subscription.id),
                                 ensure_ascii=True
                                 )

        d = flappcommand.run(stdin, self.out)
        d.addCallback(when_done)
        d.addErrback(when_failed)

    def render(self, request):
        # The expected HTTP method is a POST from the <form> in templates/subscription_signup.html.
        # render_POST is handled by the HandlerBase parent which calls this this method after logging
        # the request.

        # Get information needed to create the new stripe subscription to the S4 plan
        stripe_api_key, stripe_authorization_token, user_email = self.get_creation_parameters(request)
        try:
            # Invoke card charge by requesting subscription to recurring-payment plan.
            customer = self.create_customer(stripe_api_key, stripe_authorization_token, user_email)
        except RenderErrorDetailsForBrowser as e:
            tmpl = env.get_template('s4-subscription-form.html')
            return tmpl.render({"errorblock": e.details}).encode('utf-8', 'replace')

        # Log that a new subscription has been created (at stripe).
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        append_record(subscriptions_fp, customer.subscription.id)
        # Initiate the provisioning service
        self.run_full_signup(customer, request)
        # Return the a page notifying the user that they've been charged and their service is being set up.
        tmpl = env.get_template('payment_verified.html')
        return tmpl.render({"productfullname": "Simple Secure Storage Service", "productname":"S4"}).encode('utf-8')
