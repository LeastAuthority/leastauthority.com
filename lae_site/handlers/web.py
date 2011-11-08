import os

from jinja2 import Environment, FileSystemLoader
from twisted.web.resource import Resource


loader = FileSystemLoader(os.path.join(os.path.dirname(__file__), "../templates"))
env = Environment(loader=loader)


def jinja_render(template_name):
    tmpl = env.get_template(template_name)
    return tmpl.render().encode('ascii', 'ignore')


class JinjaHandler(Resource):
    def __init__(self, template_name):
        Resource.__init__(self)
        self.template_name = template_name

    def render(self, request):
        return self.render_GET(request)

    def render_GET(self, request):
        request.setResponseCode(200)
        return jinja_render(self.template_name)

    def getChild(self, name, request):
        return self
