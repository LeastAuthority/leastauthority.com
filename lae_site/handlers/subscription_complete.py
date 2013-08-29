
from twisted.web.resource import Resource

from lae_site.handlers.web import env

class SubscriptionReportHandler(Resource):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        pass

    def render_POST(self, request):
        import stripe, cgi
        stripe.api_key = "sk_test_mkGsLqEW6SLnZa487HYfJVLf"#'pk_test_czwzkTp2tactuLOEOqbMTRzG'
        token = request.args['stripeToken'][0]
        card = token
        plan = 's4'
        email = request.args['Email'][0]
        customer = stripe.Customer.create(card=token, plan='s4', email=request.args['Email'][0])
        return '<html><body>%s</body></html>' % (cgi.escape(customer['id']),)
