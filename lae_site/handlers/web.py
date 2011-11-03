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


class BaseResource(Resource):

    def __init__(self):
        Resource.__init__(self)

    def render(self, request):
        return self.render_GET(request)

    def getChild(self, name, request):
        return self


class Index(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('index.html')


class AboutUs(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('about_us.html')


class Support(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('support.html')


class Downloads(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('downloads.html')


class Design(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('design.html')


class Security(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('security.html')


class Products(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('products.html')


class SignUpInfo(BaseResource):

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render('sign_up_info.html')
