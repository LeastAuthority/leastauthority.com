
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

    def render(self, request):
        """
        The expected HTTP method is a POST from the <form> in templates/subscription_signup.html. 
        render_POST is handled by the HandlerBase parent which calls this this method after logging 
        the request.

        The foolscap service registered to run when flappcommand.run is called expects a bytestream
        of US-ascii valid bytes, because it is reading from its stdin (--accept-stdin flag set upon 
        addition).  Therefore the content passed to the command must conform to US-ascii.
        """
        #Parse request, info from stripe and subscriber
        stripe_authorization_token = self.get_arg(request, 'stripeToken')
        email_from_form = self.get_arg(request, 'email')

        #Load apikey
        stripefp = FilePath(self.basefp.path).child('secret_config').child('stripeapikey')
        assert (('leastauthority.com' not in stripefp.path) or ('_trial_temp' in stripefp.path)), "secrets must not be in production code repo"
        stripe_api_key = stripefp.getContent().strip()

        #invoke cc-charge by requesting subscription to recurring-payment plan
        try:
            customer = stripe.Customer.create(api_key=stripe_api_key, card=stripe_authorization_token, plan='S4', email=email_from_form)
        except stripe.CardError, e: # Errors we expect: https://stripe.com/docs/api#errors
            print >>self.out, "Got a CardError from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            tmpl = env.get_template('s4-subscription-form.html')
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')
        except stripe.APIError, e:
            print >>self.out, "Got an APIError from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            tmpl = env.get_template('s4-subscription-form.html')
            e.message = "Our payment processor is temporarily unavailable, please try again in a few moments."
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')
        except stripe.InvalidRequestError, e:
            print >>self.out, "Got an InvalidRequestError from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            headers = {
                "From": FROM_ADDRESS,
                "Subject": "InvalidRequestError Stripe API Error.",
                }
            send_plain_email('info@leastauthority.com', 'support@leastauthority.com', repr(e), headers)
            tmpl = env.get_template('s4-subscription-form.html')
            e.message = "We're having difficulty communicating with our payment processor, please submit your information again."
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')
        except Exception, e:
            print >>self.out, "Got unexpected error from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            headers = {
                "From": FROM_ADDRESS,
                "Subject": "Unexpected Error.",
                }
            send_plain_email('info@leastauthority.com', 'support@leastauthority.com', repr(e), headers)
            tmpl = env.get_template('s4-subscription-form.html')
            e.message = "We're experiencing unusual interference. Please wait awhile and try again later."
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')

        #log that a new subscription has been created (at stripe)
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        append_record(subscriptions_fp, customer.subscription.id)       

        def when_done(ignored_None):
            service_confirmed_fp = self.basefp.child(SERVICE_CONFIRMED_FILE)
            try:
                append_record(service_confirmed_fp, customer.subscription.id)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error locally.
                traceback.print_exc(100, sys.stderr)

        def when_failed(ignored_None):
            try:
                pass  #XXX  Inform operations that a subscribed customer has a broken service!!
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
        return tmpl.render({"productfullname":"Simple Secure Storage Service", "productname":"S4"}).encode('utf-8')
