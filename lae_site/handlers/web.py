import os

from jinja2 import Environment, FileSystemLoader
from twisted.web.resource import Resource

from lae_site.handlers.devpay_complete import HandlerBase


env = Environment(loader=FileSystemLoader(
                  os.path.join(
                  os.path.dirname(__file__), "../templates")))


def jinja_render(template_name):
    tmpl = env.get_template(template_name)
    return tmpl.render().encode('ascii', 'ignore')


class Index(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('index.html')


class IndexPage(Resource):
    def getChild(self, name, request):
        return Index()


class AboutUs(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('about_us.html')


class AboutPage(Resource):

    def getChild(self, name, request):
        return AboutUs()


class Support(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('support.html')


class SupportPage(Resource):

    def getChild(self, name, request):
        return Support()


class Downloads(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('downloads.html')


class DownloadsPage(Resource):

    def getChild(self, name, request):
        return Downloads()


class Design(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('design.html')


class DesignPage(Resource):

    def getChild(self, name, request):
        return Design()


class Security(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('security.html')


class SecurityPage(Resource):

    def getChild(self, name, request):
        return Security()


class Products(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('products.html')


class ProductsPage(Resource):

    def getChild(self, name, request):
        return Products()
