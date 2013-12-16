
import stripe, time, traceback, simplejson, sys

from twisted.python.filepath import FilePath

from lae_util.servers import append_record

from lae_util.timestamp import format_iso_time
from lae_util.streams import LoggingTeeStream
from lae_site.handlers.web import env
from lae_util.flapp import FlappCommand

from lae_site.handlers.main import HandlerBase

SUBSCRIPTIONS_FILE      = 'subscriptions.csv'
SERVICE_CONFIRMED_FILE  = 'service_confirmed.csv'
SIGNUP_FURL_FILE        = 'signup.furl'

flappcommand = None
all_subscribed = None
subscribed_confirmed = None

def start(basefp):
    global flappcommand, all_subscribed, subscribed_confirmed

    signup_furl_fp = basefp.child('secret_config').child(SIGNUP_FURL_FILE)
    subscriptions_fp = basefp.child(SUBSCRIPTIONS_FILE)
    service_confirmed_fp = basefp.child(SERVICE_CONFIRMED_FILE)

    flappcommand = FlappCommand(signup_furl_fp.path)
    all_subscribed = set([])
    subscribed_confirmed = set([])

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
                        subscribed_confirmed.add(subscription_id)
        finally:
            f.close()

    return flappcommand.start()


class SubscriptionReportHandler(HandlerBase):
    #XXXisLeaf = 0

    def __init__(self, basefp):
        HandlerBase.__init__(self, out=None)
        self._logger_helper(__name__)
        self.basefp = basefp

    def _create_log_filepaths(self, stripe_customer_id):
        timestamp = format_iso_time(time.time())
        fpcleantimestamp = timestamp.replace(':', '')

        logfilename = "%s-%s" % (fpcleantimestamp, stripe_customer_id)

        secretsfile_fp = self.basefp.child('secrets').child(logfilename)
        logfile_fp = self.basefp.child('signup_logs').child(logfilename)
        subscriptionsfile_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        return secretsfile_fp, logfile_fp, subscriptionsfile_fp

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
        token = self.get_arg(request, 'stripeToken')
        email_from_form = self.get_arg(request, 'email')
        customer_pgpinfo = self.get_arg(request, 'pgp_pubkey')

        #Log request info (also see site.out, signup_logs, and secrets)
        receipt_time = format_iso_time(time.time()).replace(':', '')
        reqstring = "token: %s\nemail_from_form: %s\ncustomer_pgpinfo: %s" % (token, email_from_form, customer_pgpinfo)
        self.basefp.child('secrets').child('subscription_complete_requests').child(receipt_time).setContent(reqstring)

        #invoke cc-charge by requesting subscription to recurring-payment plan
        stripefp = FilePath(self.basefp.path).child('secret_config').child('stripeapikey')
        assert (('leastauthority.com' not in stripefp.path) or ('_trial_temp' in stripefp.path)), "secrets must not be in production code repo"
        stripe_api_key = stripefp.getContent().strip()
        try:
            customer = stripe.Customer.create(api_key=stripe_api_key, card=token, plan='S4', email=email_from_form)
        except stripe.CardError, e:
            print >>self.out, "Got an exception from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            tmpl = env.get_template('subscription_signup.html')
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')

        secrets_fp, log_fp, subscriptions_fp = self._create_log_filepaths(customer.id)
        #Use of "setContent" here assures us that the associated file will be in a known state
        #when it is referenced on the other side of "the wire" <- ala foolscap
        #This is necessary because we are passing a stringified reference to the file over the wire.
        secrets_string = '\n'.join([customer.email, customer.default_card, customer.subscription.plan.name, customer.id])
        secrets_fp.setContent(secrets_string)
        log_fp.setContent(customer.email)        
        append_record(subscriptions_fp, customer.subscription.id, customer.subscription.plan.name, 
                      customer.email, customer_pgpinfo)       
        stdin = simplejson.dumps((customer.email,
                                  customer_pgpinfo,
                                  customer.id,
                                  customer.subscription.id,
                                  customer.subscription.plan.name,
                                  secrets_fp.path,
                                  log_fp.path),
                                 ensure_ascii=True
                                 )

        stdout = LoggingTeeStream(sys.stdout, log_fp.open('a'), 'stdout')
        stderr = LoggingTeeStream(sys.stderr, log_fp.open('a'), 'stderr')

        service_confirmed_fp = self.basefp.child(SERVICE_CONFIRMED_FILE)
        def when_done():
            try:
                subscribed_confirmed.add(customer.subscription.id)
                all_subscribed.add(customer.subscription.id)
                append_record(service_confirmed_fp, 'success', customer.subscription.id, 
                              customer.subscription.plan.name, customer.email, 
                              customer_pgpinfo)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error locally.
                traceback.print_exc(100, stderr)
        def when_failed():
            try:
                all_subscribed.add(customer.subscription.id)
                append_record(service_confirmed_fp, 'failure', customer.subscription.id, 
                              customer.subscription.plan.name, customer.email, 
                              customer_pgpinfo)

            except Exception:
                traceback.print_exc(100, stderr)
        try:
            flappcommand.run(stdin, stdout, stderr, when_done, when_failed)
        except Exception:
            traceback.print_exc(100, stdout)
            when_failed()

        # http://twistedmatrix.com/documents/current/web/howto/web-in-60/asynchronous.html
        tmpl = env.get_template('payment_verified.html')
        return tmpl.render({"productfullname":"Simple Secure Storage Service", "productname":"S4"}).encode('utf-8')
