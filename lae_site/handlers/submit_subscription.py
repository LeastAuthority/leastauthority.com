
import stripe, traceback, simplejson, sys

from twisted.python.filepath import FilePath

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

class SubmitSubscriptionHandler(HandlerBase):

    def __init__(self, basefp):
        HandlerBase.__init__(self, out=None)
        self._logger_helper(__name__)
        self.basefp = basefp

    def create_cust_errhandler(self, trace_back, error, details, email_subject):
        error.details = details
        print >>self.out, "Got %s from the stripe.Customer.create call:" % (error.__class__.__name__,)
        print >>self.out, trace_back
        headers = {
            "From": FROM_ADDRESS,
            "Subject": email_subject,
        }
        send_plain_email('info@leastauthority.com', 'support@leastauthority.com', trace_back, headers)
        raise error

    def create_customer(self, stripe_api_key, stripe_authorization_token, email_from_form):
        try:
            return stripe.Customer.create(api_key=stripe_api_key, card=stripe_authorization_token, plan='S4', 
                                          email=email_from_form)
        except stripe.CardError as e:
            # Errors we expect: https://stripe.com/docs/api#errors
            self.create_cust_errhandler(traceback.format_exc(100), e,
                                        details=e.message,
                                        email_subject="Stripe Card error")
        except stripe.APIError as e:
            self.create_cust_errhandler(traceback.format_exc(100), e,
                                        details="Our payment processor is temporarily unavailable,"+\
                                            " please try again in\ a few moments.",
                                        email_subject="Stripe API error")
        except stripe.InvalidRequestError as e:
            self.create_cust_errhandler(traceback.format_exc(100), e,
                                        details="Due to technical difficulties unrelated to your card"+\
                                            " details, we were unable to charge your account. Our"+\
                                            " engineers have been notified and will contact you with"+\
                                            " an update shortly.",
                                        email_subject="Stripe Invalid Request error")
        except Exception as e:
            self.create_cust_errhandler(traceback.format_exc(100), e,
                                        details="Something went wrong. Please try again, or contact"+\
                                            " <support@leastauthority.com>.",
                                        email_subject="Stripe unexpected error")

    def render(self, request):
        # The expected HTTP method is a POST from the <form> in templates/subscription_signup.html.
        # render_POST is handled by the HandlerBase parent which calls this this method after logging
        # the request.
        #
        # The foolscap service registered to run when flappcommand.run is called expects a bytestream
        # of US-ASCII bytes, because it is reading from its stdin (--accept-stdin flag set).
        # Therefore the content passed to the command must conform to US-ASCII.

        # Parse request, info from stripe and subscriber.
        stripe_authorization_token = self.get_arg(request, 'stripeToken')
        email_from_form = self.get_arg(request, 'email')

        # Load apikey.
        stripefp = FilePath(self.basefp.path).child('secret_config').child('stripeapikey')
        if ('leastauthority.com' in stripefp.path) and ('_trial_temp' not in stripefp.path):
            raise AssertionError("secrets must not be in production code repo: %r" % (stripefp.path,))
        stripe_api_key = stripefp.getContent().strip()

        # Invoke card charge by requesting subscription to recurring-payment plan.
        try:
            customer = self.create_customer(stripe_api_key, stripe_authorization_token, email_from_form)
        except Exception as e:
            tmpl = env.get_template('s4-subscription-form.html')
            return tmpl.render({"errorblock": e.details}).encode('utf-8', 'replace')

        # Log that a new subscription has been created (at stripe).
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        append_record(subscriptions_fp, customer.subscription.id)

        def when_done(ign):
            service_confirmed_fp = self.basefp.child(SERVICE_CONFIRMED_FILE)
            try:
                append_record(service_confirmed_fp, customer.subscription.id)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error locally.
                traceback.print_exc(100, sys.stderr)

        def when_failed(ign):
            try:
                headers = {
                    "From": FROM_ADDRESS,
                    "Subject": "Sign-up error",
                }
                send_plain_email('info@leastauthority.com', 'support@leastauthority.com',
                                 "A sign-up failed for <%s>." % (email_from_form,), headers)
            except Exception:
                traceback.print_exc(100, sys.stderr)

        customer_pgpinfo = self.get_arg(request, 'pgp_pubkey')
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

        tmpl = env.get_template('payment_verified.html')
        return tmpl.render({"productfullname": "Simple Secure Storage Service", "productname":"S4"}).encode('utf-8')
