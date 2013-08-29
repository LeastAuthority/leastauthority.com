
from twisted.web.resource import Resource

from lae_site.handlers.web import env

class SubscriptionSubmitHandler(Resource):
    #XXXisLeaf = 0

    def __init__(self, basefp, products):
        pass

    def render_GET(self, request):
        tmpl = env.get_template('subscription_signup.html')
        return tmpl.render().encode('utf-8', 'replace')
