
from twisted.web.util import Redirect
from twisted.web.resource import Resource


class SignupHandler(Resource):
    isLeaf = 0

    def __init__(self, products):
        self.products = products
        self.children = {}
        self.putChild('', Redirect('/products'))
        for product in self.products:
            self.putChild(str(product['short_name']), Redirect(str(product['signup_url'])))

    def render_GET(self, request):
        tmpl = env.get_template('products.html')
        request.setResponseCode(200)
        return tmpl.render().encode('utf-8', 'replace')
