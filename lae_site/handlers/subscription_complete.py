
import stripe, time, traceback, simplejson

from twisted.web.server import NOT_DONE_YET

from lae_util.servers import append_record
from lae_util.flapp import FlappCommand
from lae_util.timestamp import format_iso_time
from lae_automation.server import create_secrets_file
from lae_site.handlers.web import env
from lae_site.handlers.devpay_complete import HandlerBase, SUCCEEDED_HTML,FAILED_HTML


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

    def __init__(self, basefp, products, out=None):
        HandlerBase.__init__(self, out=out)
        self.basefp = basefp

    def render(self, request):
        """
        Some of the values returned either from parsing the request or from stripe
        are unicodes.   Foolscaphas been handling unicodes poorly.
        """
        stripe.api_key = "sk_test_mkGsLqEW6SLnZa487HYfJVLf"
        token = request.args['stripeToken'][0]
        try:
            customer = stripe.Customer.create(card=token, plan='S4', email=request.args['email'][0])
        except stripe.CardError, e:
            #return "<html><head></head><body><h2>Stripe Processing Error</h2>%s</body></html>" % str(e)
            print >>self.out, "Got an exception from the stripe.Customer.create call:"
            #print >>self.out, type(e)
            print >>self.out, dir(e)
            print >>self.out, repr(e)
            tmpl = env.get_template('subscription_signup.html')
            return tmpl.render({"errorblock": e.message}).encode('utf-8', 'replace')
        nickname = request.args['nickname'][0]
        timestamp = format_iso_time(time.time())
        fpcleantimestamp = timestamp.replace(':', '')
        secrets_fp, log_fp = create_secrets_file(self.basefp, fpcleantimestamp, customer.id)
        subscriptions_fp = self.basefp.child(SUBSCRIPTIONS_FILE)
        #Use of "setContent" here assures us that the associated file will be in a known state
        #when it is referenced on the other side of "the wire" <- ala foolscap
        #This is necessary because we are passing a stringified reference to the file over the wire.
        secrets_fp.setContent('\n'.join([customer.email, customer.default_card, 
                                         customer.subscription.plan.name, customer.id]))
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

        stdout = self.out
        stderr = self.out

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
                request.write(SUCCEEDED_HTML)
            else:
                request.write(SUCCEEDED_HTML)
            finally:
                request.finish()
        def when_failed():
            try:
                all_subscribed.add(customer.subscription.id)
                append_record(service_confirmed_fp, 'failure', customer.subscription.id, 
                              customer.subscription.plan.name, nickname, customer.email, 
                              customer_pgpinfo)

            except Exception:
                traceback.print_exc(100, stderr)
                request.write(FAILED_HTML)
            else:
                request.write(FAILED_HTML)
            finally:
                request.finish()
        try:
            flappcommand.run(stdin, stdout, stderr, when_done, when_failed)
        except Exception:
            traceback.print_exc(100, stdout)
            when_failed()

        # http://twistedmatrix.com/documents/10.1.0/web/howto/web-in-60/asynchronous.html
        
        #return '<html><body>%s<br>%s</body></html>' % (cgi.escape(request.args.__repr__()),'foo')#cgi.escape((customer))))

        return NOT_DONE_YET
