# TODO: Remove JinjaHadlers and templates

from jinja2 import Environment, FileSystemLoader
from jinja2.exceptions import TemplateNotFound

from twisted.web.resource import Resource
from twisted.python.filepath import FilePath

loader = FileSystemLoader(
    FilePath(__file__).parent().parent().child("templates").path,
)
env = Environment(loader=loader)


class JinjaHandler(Resource):
    def __init__(self, template_name):
        Resource.__init__(self)
        self.template_name = template_name

    def render(self, request):
        return self.render_GET(request)

    def render_GET(self, request):
        try:
            tmpl = env.get_template(self.template_name)
        except TemplateNotFound:
            tmpl = env.get_template('notfound.html')
            request.setResponseCode(404)
        else:
            request.setResponseCode(200)

        return tmpl.render().encode('utf-8', 'replace')

    def getChild(self, name, request):
        if not name:
            return self
        elif self.template_name == 'index.html':
            return JinjaHandler(name + '.html')
        else:
            return JinjaHandler('')
