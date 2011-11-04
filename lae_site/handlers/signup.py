
from twisted.web.util import Redirect
from twisted.web.resource import Resource


class SignupHandler(Resource):
    isLeaf = 0

    def __init__(self, products):
        self.products = products
        self.children = {}
        for product in self.products:
            self.putChild(str(product['short_name']), Redirect(str(product['signup_url'])))

    def render_GET(self, request):
        # TODO: prettify and make valid HTML
        return str('<html><body>Please select a product:<ul>%s</ul></body></html>'
                   % (''.join(['<li><a href="/signup/%s">%s</a>' % (p['short_name'], p['full_name']) for p in self.products]),))
