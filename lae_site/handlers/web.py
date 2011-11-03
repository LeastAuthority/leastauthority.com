import os

from jinja2 import Environment, FileSystemLoader
from twisted.web.resource import Resource

from lae_site.handlers.devpay_complete import HandlerBase


env = Environment(loader=FileSystemLoader(os.path.join(os.path.dirname(__file__), "../templates")))

class Index(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('index.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class IndexPage(Resource):
    def getChild(self, name, request):
        return Index()


class AboutUs(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('about_us.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class AboutPage(Resource):
    
    def getChild(self, name, request):
        return AboutUs()


class Support(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('support.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class SupportPage(Resource):
    
    def getChild(self, name, request):
        return Support()


class Downloads(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('downloads.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class DownloadsPage(Resource):
    
    def getChild(self, name, request):
        return Downloads()


class Design(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('design.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class DesignPage(Resource):
    
    def getChild(self, name, request):
        return Design()


class Security(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('security.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class SecurityPage(Resource):
    
    def getChild(self, name, request):
        return Security()


class Products(Resource):

    def __init__(self):
        Resource.__init__(self)
    
    def render_GET(self, request):
        tmpl = env.get_template('products.html')
        request.setResponseCode(200)
        return tmpl.render().encode('ascii', 'ignore')


class ProductsPage(Resource):
    
    def getChild(self, name, request):
        return Products()


