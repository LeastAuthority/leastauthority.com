from twisted.web.resource import Resource

import traceback

import attr

from twisted.logger import Logger
from twisted.web.server import NOT_DONE_YET

from lae_util import stripe
from lae_util.send_email import send_plain_email, FROM_ADDRESS

from lae_site.handlers.web import env

from lae_site.handlers.main import HandlerBase
from lae_site.handlers.s4_signup_style import S4_SIGNUP_STYLE_COOKIE

PLAN_ID                 = u'S4_consumer_iteration_2_beta1_2014-05-27'

logger = Logger()


class RenderErrorDetailsForBrowser(Exception):
    def __init__(self, details):
        self.details = details

@attr.s
class Stripe(object):
    key = attr.ib()
    # email = "vtsoneva@gmail.com"
    def create(self, authorization_token, plan_id, email):
        return stripe.Customer.create(
            api_key=self.key,
            card=authorization_token,
            plan=plan_id,
            email=email,
        )

class Mailer(object):
    def mail(self, from_addr, to_addr, subject, headers):
        return send_plain_email(
            from_addr, to_addr, subject, headers,
        )

class CreateSubscription(HandlerBase):
  def addHeaders(self, request):
    request.setHeader('Access-Control-Allow-Origin', 'http://localhost:5000')
    return request

  def __init__(self, get_signup, mailer, stripe):
    """

    :param get_signup: A one-argument callable which returns an ``ISignup``
        which can sign up a new user for us.  The argument is the kind of
        signup desired, ``wormhole`` or ``email``.
    """
    HandlerBase.__init__(self, out=None)
    self._logger_helper(__name__)
    self._get_signup = get_signup
    self._mailer = mailer
    self._stripe = stripe
    # The following helper methods are all called directly or indirectly by render.

  def handle_stripe_create_customer_errors(self, trace_back, error, details, email_subject, notes=''):
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

  def create_customer(self, stripe_authorization_token, user_email, request):
        try:
            return self._stripe.create(
                authorization_token=stripe_authorization_token,
                plan_id=PLAN_ID,
                email=user_email,
                # request=request
            )
        except stripe.CardError as e:
            # Always return 402 on card errors
            request.setResponseCode(402)
            # Errors we expect: https://stripe.com/docs/api#errors
            note = "Note: This error could be caused by insufficient funds, or other charge-disabling "+\
                "factors related to the User's payment credential.\n"
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
              details=e.message.encode("utf-8"),
              email_subject="Stripe Card error",
              notes=note)
        except stripe.APIError as e:
            # Should return the same as Authentication error
            request.setResponseCode(401)
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
              details="Our payment processor is temporarily unavailable,"+
                  " please try again in\ a few moments.",
              email_subject="Stripe API error")
        except stripe.InvalidRequestError as e:
            # Return 422 - unusable entity error
            request.setResponseCode(422)
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
              details="Due to technical difficulties unrelated to your card"+
                  " details, we were unable to charge your account. Our"+
                  " engineers have been notified and will contact you with"+
                  " an update shortly.",
              email_subject="Stripe Invalid Request error")
        except stripe.AuthenticationError as e:
            request.setResponseCode(401)
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
              details="Our payment processor is temporarily unavailable,"+
                  " please try again in\ a few moments.",
              email_subject="Stripe Auth error")
        except Exception as e:
            # Return a generic error here
            request.setResponseCode(400)
            self.handle_stripe_create_customer_errors(traceback.format_exc(100), e,
              details="Something went wrong. Please try again, or contact"+
                "support@leastauthority.com.",
              email_subject="Stripe unexpected error")

  def render_POST(self, request):
    request = self.addHeaders(request)
    stripe_authorization_token = request.args.get(b"stripeToken")[0]
    user_email = request.args.get(b"email")[0]

    try:
        # Invoke card charge by requesting subscription to recurring-payment plan.
        customer = self.create_customer(stripe_authorization_token, user_email, request)
    except RenderErrorDetailsForBrowser as e:
        return e.details

    # Initiate the provisioning service
    subscription = customer.subscriptions.data[0]
    style = (request.getCookie(S4_SIGNUP_STYLE_COOKIE) or "email").decode("ascii")
    signup = self._get_signup(style)
    d = signup.signup(
        customer.email.decode("utf-8"),
        customer.id.decode("utf-8"),
        subscription.id.decode("utf-8"),
        subscription.plan.id.decode("utf-8"),
    )
    d.addCallback(signed_up, request)
    d.addErrback(signup_failed, customer, self._mailer)
    return NOT_DONE_YET

def signed_up(claim, request):
    # Return 200 and text to be added to template on client
    request.write(claim.describe(env).encode('utf-8'))
    request.finish()

def signup_failed(request, reason, customer, mailer):
  # request.setResponseCode(400)
  headers = {
      "From": FROM_ADDRESS,
      "Subject": "Sign-up error",
  }
  logger.failure("Signup failed", reason)
  mailer.mail(
      'info@leastauthority.com',
      'support@leastauthority.com',
      "A sign-up failed for <%s>." % (customer.email,),
      headers,
  )