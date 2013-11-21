
import stripe, time, traceback, simplejson, sys

from twisted.web.server import NOT_DONE_YET
from twisted.python.filepath import FilePath

from lae_util.servers import append_record
from lae_util.flapp import FlappCommand
from lae_util.timestamp import format_iso_time
from lae_util.streams import LoggingTeeStream
from lae_automation.server import create_secrets_file
from lae_site.handlers.web import env
from lae_site.handlers.devpay_complete import HandlerBase, RequestOutputStream


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
        self.basefp = basefp

    def _delayedRender(self, request):
        tmpl = env.get_template('payment_verified.html')
        request.write(tmpl.render({"productfullname":"Simple Secure Storage Service", "productname":"S4"}).encode('utf-8'))
        request.finish()

    def _responseFailed(self, failure, call):
        call.cancel()
        
    def render(self, request):
        """
        The expected HTTP method is a POST from the <form> in templates/subscription_signup.html. 
        render_POST is handled by the HandlerBase parent which calls this this method after logging 
        the request.

        The foolscap service registered to run when flappcommand.run is called expects a bytestream
        of US-ascii valid bytes, because it is reading from its stdin (--accept-stdin flag set upon 
        addition).  Therefore the content passed to the command must conform to US-ascii.
        """
        stripefp = FilePath(self.basefp.path).child('secret_config').child('stripeapikey')
        assert (('leastauthority.com' not in stripefp.path) or ('_trial_temp' in stripefp.path)), "secrets must not be in production code repo"
        stripe_api_key = stripefp.getContent().strip()
        token = request.args['stripeToken'][0]
        try:
            customer = stripe.Customer.create(api_key=stripe_api_key, card=token, plan='S4', email=request.args['email'][0])
        except stripe.CardError, e:
            print >>self.out, "Got an exception from the stripe.Customer.create call:"
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            tmpl = env.get_template('subscription_signup.html')
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')
        from twisted.internet import reactor
        call = reactor.callLater(1, self._delayedRender, request)
        request.notifyFinish().addErrback(self._responseFailed, call)
        nickname = request.args['nickname'][0]
        timestamp = format_iso_time(time.time())
        fpcleantimestamp = timestamp.replace(':', '')
        secrets_fp, log_fp = create_secrets_file(self.basefp, fpcleantimestamp, customer.id)
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        #Use of "setContent" here assures us that the associated file will be in a known state
        #when it is referenced on the other side of "the wire" <- ala foolscap
        #This is necessary because we are passing a stringified reference to the file over the wire.
        secrets_string = '\n'.join([customer.email, customer.default_card, customer.subscription.plan.name, customer.id])
        secrets_fp.setContent(secrets_string)
        RequestOutputStream(request, tee=secrets_fp.open('a'))
        log_fp.setContent(customer.email)
        
        customer_pgpinfo = request.args['pgp_pubkey'][0]
        
        append_record(subscriptions_fp, customer.subscription.id, customer.subscription.plan.name, 
                      nickname, customer.email, customer_pgpinfo)       
        
        stdin = simplejson.dumps((nickname,
                                 customer.email,
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
                              customer.subscription.plan.name, nickname, customer.email, 
                              customer_pgpinfo)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error locally.
                traceback.print_exc(100, stderr)
        def when_failed():
            try:
                all_subscribed.add(customer.subscription.id)
                append_record(service_confirmed_fp, 'failure', customer.subscription.id, 
                              customer.subscription.plan.name, nickname, customer.email, 
                              customer_pgpinfo)

            except Exception:
                traceback.print_exc(100, stderr)
        try:
            flappcommand.run(stdin, stdout, stderr, when_done, when_failed)
        except Exception:
            traceback.print_exc(100, stdout)
            when_failed()

        # http://twistedmatrix.com/documents/current/web/howto/web-in-60/asynchronous.html
        return NOT_DONE_YET
