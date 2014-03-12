
from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import redirectTo, Redirect
from twisted.web.resource import Resource

from lae_site.handlers.devpay_complete import CollectEmailHandler, DevPayPurchaseHandler, ActivationRequestHandler
from lae_site.handlers.signup import SignupHandler
from lae_site.handlers.web import JinjaHandler
from lae_site.handlers.subscribing import SubscriptionSubmitHandler
from lae_site.handlers.subscription_complete import SubscriptionReportHandler


def make_site(basefp, config):
    resource = JinjaHandler('index.html')
    resource.putChild('static', File('content/static'))
    resource.putChild('blog', File('content/blog'))
    resource.putChild('collect-email', CollectEmailHandler(basefp, config.products))
    resource.putChild('signup', SignupHandler(config.products)) # is this obsolete now? the stripe processing is all in 'subscribing.py' right?
    resource.putChild('devpay-complete', DevPayPurchaseHandler(basefp, config.products))
    resource.putChild('activation-request', ActivationRequestHandler(basefp, config.products))
    resource.putChild('subscribing', SubscriptionSubmitHandler(basefp, config.products))
    resource.putChild('subscription-complete', SubscriptionReportHandler(basefp, config.products))
    resource.putChild('support', Redirect("https://leastauthority.zendesk.com/home"))

    site = Site(resource, logPath=basefp.child('sitelogs').path)
    site.displayTracebacks = False
    return site


EXPECTED_DOMAIN = 'leastauthority.com'

class RedirectToHTTPS(Resource):
    """
    I redirect to the same path at https:, rewriting *.EXPECTED_DOMAIN -> EXPECTED_DOMAIN.
    Thanks to rakslice at http://stackoverflow.com/questions/5311229/redirect-http-to-https-in-twisted
    """
    isLeaf = 0

    def __init__(self, port, *args, **kwargs):
        Resource.__init__(self, *args, **kwargs)
        self.port = port

    def render(self, request):
        newpath = request.URLPath()
        assert newpath.scheme != "https", "https->https redirection loop: %r" % (request,)
        newpath.scheme = "https"
        host = newpath.netloc.split(':')[0]
        if host.endswith('.' + EXPECTED_DOMAIN):
            host = EXPECTED_DOMAIN
        if self.port == 443:
            newpath.netloc = host
        else:
            newpath.netloc = "%s:%d" % (host, self.port)
        return redirectTo(newpath, request)

    def getChild(self, name, request):
        return self


def make_redirector_site(port):
    site = Site(RedirectToHTTPS(port))
    site.displayTracebacks = False
    return site
