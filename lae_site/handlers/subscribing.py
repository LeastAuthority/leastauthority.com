
from lae_site.handlers.web import env
from lae_site.handlers.main import HandlerBase

class SubscriptionSubmitHandler(HandlerBase):
    #XXXisLeaf = 0

    def __init__(self, basefp):
        pass

    def render_GET(self, request):
        tmpl = env.get_template('subscription_signup.html')
        email = self.get_arg(request, 'email')
        return tmpl.render(email=email).encode('utf-8', 'replace')
