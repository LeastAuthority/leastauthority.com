
#XXXfrom twisted.web.util import Redirect
import stripe
from twisted.web.resource import Resource

from lae_site.handlers.web import env

class SubscriptionHandler(Resource):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        pass
        """self.products = products
        self.children = {}
        self.putChild('', Redirect('/products'))
        for product in self.products:
            self.putChild(str(product['short_name']), Redirect(str(product['signup_url'])))"""

    def render_GET(self, request):
        tmpl = env.get_template('subscription_signup.html')
        return tmpl.render().encode('utf-8', 'replace')

    def render_POST(self, request):
        return '<html><body>%s</body></html>' % (request.args,)
