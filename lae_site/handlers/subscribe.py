
#XXXfrom twisted.web.util import Redirect
from twisted.web.resource import Resource

from lae_site.handlers.web import env

class SubscriptionHandler(Resource):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        pass

    def render_GET(self, request):
        tmpl = env.get_template('subscription_signup.html')
        return tmpl.render().encode('utf-8', 'replace')

    def render_POST(self, request):
        import stripe
        stripe.api_key = "sk_test_mkGsLqEW6SLnZa487HYfJVLf"#'pk_test_czwzkTp2tactuLOEOqbMTRzG'
        token = request.args['stripeToken'][0]
        card = token
        plan = 's4'
        email = request.args['Email'][0]
        customer = stripe.Customer.create(card=token, plan='s4', email=request.args['Email'][0])
        return '<html><body>%s</body></html>' % (customer['id'],)
