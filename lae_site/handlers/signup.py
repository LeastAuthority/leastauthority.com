
from twisted.web.util import Redirect
from twisted.web.resource import Resource

from lae_site.handlers.web import env


class SignupHandler(Resource):
    isLeaf = 0

    def __init__(self, products):
        self.products = products
        self.children = {}
        for product in self.products:
            self.putChild(str(product['short_name']), Redirect(str(product['signup_url'])))

    def render_GET(self, request):
        # FIXME: escape the short and full names
        productlist = ''.join(['<li><a href="/signup/%s">%s</a>' % (p['short_name'], p['full_name'])
                               for p in self.products if p['listed']])

        tmpl = env.get_template('signup.html')
        request.setResponseCode(200)
        return tmpl.render(productlist=productlist).encode('ascii', 'ignore')