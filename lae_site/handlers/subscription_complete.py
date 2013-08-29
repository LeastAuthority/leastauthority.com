
import stripe, cgi, simplejson, time

from lae_util.timestamp import format_iso_time
from lae_automation.server import create_secrets_file
from lae_site.handlers.web import env
from lae_site.handlers.devpay_complete import HandlerBase

class SubscriptionReportHandler(HandlerBase):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        self.basefp = basefp

    def render_POST(self, request):
        stripe.api_key = "sk_test_mkGsLqEW6SLnZa487HYfJVLf"
        token = request.args['stripeToken'][0]
        customer = stripe.Customer.create(card=token, plan='s4', email=request.args['Email'][0])
        timestamp = format_iso_time(time.time()).replace(':', '')
        secrets_fh, log_fh = create_secrets_file(self.basefp, timestamp)
        print >> secrets_fh, customer.id
        return '<html><body>%s<br>%s</body></html>' % (cgi.escape(request.args.__repr__()),'foo')#cgi.escape((customer))))
