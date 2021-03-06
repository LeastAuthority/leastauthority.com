import traceback
from json import dumps

import attr

import chargebee

from twisted.logger import Logger
from twisted.web.server import NOT_DONE_YET
from twisted.web.http import (
    PAYMENT_REQUIRED,
    UNAUTHORIZED,
    BAD_REQUEST,
)

from lae_util import stripe
from lae_util.send_email import send_plain_email, FROM_ADDRESS

from lae_site.handlers.web import env

from lae_site.handlers.main import HandlerBase

logger = Logger()

# global variable for signup style wormhole
s4_signup_style = 'wormhole'

class RenderErrorDetailsForBrowser(Exception):
    def __init__(self, details):
        self.details = details



_EU_COUNTRIES = {
    "be",
    "bg",
    "cz",
    "dk",
    "de",
    "ee",
    "ie",
    "el",
    "es",
    "fr",
    "hr",
    "it",
    "cy",
    "lv",
    "lt",
    "lu",
    "hu",
    "mt",
    "nl",
    "at",
    "pl",
    "pt",
    "ro",
    "si",
    "sk",
    "fi",
    "se",
    "uk",
}



class NonEUCountryError(Exception):
    """
    An EU member country was required but a non-EU member country was
    provided.
    """



def _in_eu(code):
    if code is None:
        raise ValueError("Country code may not be None")
    # http://publications.europa.eu/code/pdf/370000en.htm#pays
    code = code.decode("ascii").lower()
    if code in _EU_COUNTRIES:
        return code.upper()
    raise NonEUCountryError(code)



@attr.s
class EUCountry(object):
    country_code = attr.ib(
        validator=attr.validators.instance_of(unicode),
        convert=_in_eu,
    )

    def add(self, parameters):
        parameters["billing_address"] = {
            "country": self.country_code,
        }
        return parameters



class NonEUCountry(object):
    def add(self, parameters):
        return parameters



@attr.s
class ChargeBee(object):
    # Notes:
    #
    # Must have Settings > Payment Gateways > Smart Routing for currency of
    # credit cards to be handled.
    #
    # Must have plan_id in Product Catalog > Plans.
    #
    # ChargeBee account must be configured to use same Stripe account as
    # payment gateway as web server is configured to use for signup form.
    #
    # ChargeBee must not be configured to require any additional card details
    # (they're not (presently?) available via the Stripe integration).

    key = attr.ib()
    name = attr.ib()
    gateway_account_id = attr.ib()
    default_plan_id = attr.ib()

    def create(self, authorization_token, plan_id, email, country):
        subscription_parameters = {
            "plan_id": plan_id,
            "customer": {
                "email": email,
            },
            "payment_method": {
                "type": "card",
                "tmp_token": authorization_token,
                "gateway_account_id": self.gateway_account_id,
            },
        }
        subscription_parameters = country.add(subscription_parameters)

        chargebee.configure(self.key, self.name)
        subscription = chargebee.Subscription.create(
            subscription_parameters,
        )
        return SubscriptionResult(
            customer_email=subscription.customer.email.decode("utf-8"),
            customer_id=subscription.customer.id.decode("utf-8"),
            subscription_id=subscription.subscription.id.decode("utf-8"),
            plan_id=subscription.subscription.plan_id.decode("utf-8"),
        )



@attr.s
class Stripe(object):
    key = attr.ib()
    default_plan_id = attr.ib()

    def create(self, authorization_token, plan_id, email, country):
        customer = stripe.Customer.create(
            api_key=self.key,
            card=authorization_token,
            plan=plan_id,
            email=email,
        )
        subscription = customer.subscriptions.data[0]
        return SubscriptionResult(
            customer_email=customer.email.decode("utf-8"),
            customer_id=customer.id.decode("utf-8"),
            subscription_id=subscription.id.decode("utf-8"),
            plan_id=subscription.plan.id.decode("utf-8"),
        )



@attr.s
class SubscriptionResult(object):
    customer_email = attr.ib()
    customer_id = attr.ib()
    subscription_id = attr.ib()
    plan_id = attr.ib()



@attr.s
class Mailer(object):
    from_addr = attr.ib()
    to_addr = attr.ib()

    def mail(self, subject, headers):
        return send_plain_email(
            self.from_addr, self.to_addr, subject, headers,
        )

class CreateSubscription(HandlerBase):
    def __init__(self, get_signup, mailer, billing, content_type):
        """
        :param get_signup: A one-argument callable which returns an ``ISignup``
            which can sign up a new user for us.  The argument is the kind of
            signup desired, ``wormhole`` or ``email``.
        """
        HandlerBase.__init__(self, out=None)
        self._logger_helper(__name__)
        self._get_signup = get_signup
        self._mailer = mailer
        self._billing = billing
        self._content_type = content_type


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
        self._mailer.mail(
            body,
            headers,
        )
        raise RenderErrorDetailsForBrowser(details)

    def create_customer(self, stripe_authorization_token, user_email, plan_id, country, request):
        PaymentError = (chargebee.PaymentError, stripe.CardError)
        OperationError = (chargebee.OperationFailedError, stripe.APIError)
        RequestError = (chargebee.InvalidRequestError, stripe.InvalidRequestError)
        AuthenticationError = (chargebee.APIError, stripe.AuthenticationError)

        try:
            return self._billing.create(
                authorization_token=stripe_authorization_token,
                plan_id=plan_id,
                email=user_email,
                country=country,
            )
        except PaymentError as e:
            # Always return 402 on card errors
            request.setResponseCode(PAYMENT_REQUIRED)
            # Errors we expect: https://stripe.com/docs/api#errors
            note = (
                "Note: This error could be caused by insufficient funds, or other charge-disabling "
                "factors related to the User's payment credential."
            )
            self.handle_stripe_create_customer_errors(
                traceback.format_exc(100), e,
                details=e.message.encode("utf-8"),
                email_subject="Stripe Card error ({})".format(user_email),
                notes=note,
            )
        except OperationError as e:
            # Should return the same as Authentication error
            request.setResponseCode(UNAUTHORIZED)
            self.handle_stripe_create_customer_errors(
                traceback.format_exc(100), e,
                details=(
                    "Our payment processor is temporarily unavailable, "
                    "please try again in a few moments."
                ),
                email_subject="Stripe API error ({})".format(user_email),
            )
        except RequestError as e:
            # Return 422 - unusable entity error
            request.setResponseCode(422)
            self.handle_stripe_create_customer_errors(
                traceback.format_exc(100), e,
                details=(
                    "Due to technical difficulties unrelated to your card "
                    "details, we were unable to charge your account. Our "
                    "engineers have been notified and will contact you with "
                    "an update shortly."
                ),
                email_subject="Stripe Invalid Request error ({})".format(user_email),
            )
        except AuthenticationError as e:
            request.setResponseCode(UNAUTHORIZED)
            self.handle_stripe_create_customer_errors(
                traceback.format_exc(100), e,
                details=(
                    "Our payment processor is temporarily unavailable, "
                    "please try again in a few moments."
                ),
                email_subject="Stripe Auth error ({})".format(user_email),
            )
        except Exception as e:
            # Return a generic error here
            request.setResponseCode(BAD_REQUEST)
            self.handle_stripe_create_customer_errors(
                traceback.format_exc(100), e,
                details=(
                    "Something went wrong. Please try again, or contact "
                    "support@leastauthority.com."
                ),
                email_subject="Stripe unexpected error ({})".format(user_email),
            )

    def render_POST(self, request):
        stripe_authorization_token = request.args.get(b"stripeToken")[0]
        user_email = request.args.get(b"email")[0]

        country = NonEUCountry()
        try:
            # Two-byte country identifier - eg US or CN
            country_code = request.args[b"country"][0]
        except KeyError:
            pass
        else:
            try:
                country = EUCountry(country_code)
            except NonEUCountryError:
                pass

        plan_id = request.args.get(b"plan-id", [self._billing.default_plan_id])[0]
        if plan_id not in {self._billing.default_plan_id}:
            return "Invalid `plan-id` given."

        try:
            # Invoke card charge by requesting subscription to recurring-payment plan.
            result = self.create_customer(
                stripe_authorization_token,
                user_email,
                plan_id,
                country,
                request,
            )
        except RenderErrorDetailsForBrowser as e:
            if self._content_type == u"text/html":
                return e.details
            elif self._content_type == u"application/json":
                return dumps({"v1": {"error": e.details}})

        # Initiate the provisioning service
        style = s4_signup_style.decode("ascii")
        signup = self._get_signup(style)
        d = signup.signup(
            result.customer_email,
            result.customer_id,
            result.subscription_id,
            result.plan_id,
        )
        d.addCallback(signed_up, request, self._content_type)
        d.addErrback(signup_failed, result, self._mailer)
        return NOT_DONE_YET

def signed_up(claim, request, content_type):
    # Return 200 and text to be added to template on client
    request.responseHeaders.setRawHeaders(
        "content-type",
        [content_type],
    )
    request.write(
        claim.describe(env, content_type).encode('utf-8'),
    )
    request.finish()

def signup_failed(reason, result, mailer):
    headers = {
        "From": FROM_ADDRESS,
        "Subject": "Sign-up error",
    }
    logger.failure("Signup failed", reason)
    mailer.mail(
        "A sign-up failed for <%s>." % (result.customer_email,),
        headers,
    )
