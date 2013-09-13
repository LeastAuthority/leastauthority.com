
import stripe, cgi, time, sys

from lae_util.servers import append_record
from lae_util.flapp import FlappCommand
from lae_util.timestamp import format_iso_time
from lae_automation.server import create_secrets_file
from lae_automation.signup import activate_subscribed_service
#from lae_site.handlers.web import env
from lae_site.handlers.devpay_complete import HandlerBase

SUBSCRIPTIONS_FILE      = 'subscriptions.csv'
SERVICE_CONFIRMED_FILE  = 'service_confirmed.csv'
SIGNUP_FURL_FILE        = 'signup.furl'

def start(basefp):
    global flappcommand, all_subscribed, subcribed_confirmed

    signup_furl_fp = basefp.child('secret_config').child(SIGNUP_FURL_FILE)
    subscriptions_fp = basefp.child(SUBSCRIPTIONS_FILE)
    service_confirmed_fp = basefp.child(SERVICE_CONFIRMED_FILE)

    flappcommand = FlappCommand(signup_furl_fp.path)
    all_subscribed = set([])
    subcribed_confirmed = set([])

    # read the sets of keys
    try:
        f = subscriptions_fp.open("r")
    except IOError:
        if subscriptions_fp.exists():
            raise
    else:
        try:
            for line in f:
                fields = line.split(',')
                if len(fields) >= 2:
                    [timestamp, subscription_id] = fields[:2]
                    all_subscribed.add(subscription_id)
        finally:
            f.close()

    try:
        f = service_confirmed_fp.open("r")
    except IOError:
        if service_confirmed_fp.exists():
            raise
    else:
        try:
            for line in f:
                fields = line.split(',')
                if len(fields) >= 3:
                    [timestamp, outcome, subscription_id] = fields[:3]
                    if outcome == "success":
                        subcribed_confirmed.add(subscription_id)
        finally:
            f.close()

    return flappcommand.start()

class SubscriptionReportHandler(HandlerBase):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        self.basefp = basefp

    def render_POST(self, request):
        stripe.api_key = "sk_test_mkGsLqEW6SLnZa487HYfJVLf"
        token = request.args['stripeToken'][0]
        customer = stripe.Customer.create(card=token, plan='s4', email=request.args['Email'][0])
        timestamp = format_iso_time(time.time())
        fpcleantimestamp = timestamp.replace(':', '')
        secrets_fh, log_fh = create_secrets_file(self.basefp, fpcleantimestamp, customer.id)
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        print >> secrets_fh, customer.email
        print >> secrets_fh, customer.default_card
        print >> secrets_fh, customer.subscription.status
        print >> secrets_fh, customer.subscription.plan.name
        print >> log_fh, customer.email
        nickname = 'TEST'
        append_record(subscriptions_fp, customer.subscription.id, customer.subscription.plan.name.replace(' ', '_'), nickname, customer.email)
        

        return '<html><body>%s<br>%s</body></html>' % (cgi.escape(request.args.__repr__()),'foo')#cgi.escape((customer))))
