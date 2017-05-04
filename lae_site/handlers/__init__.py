from json import dumps

from twisted.web.server import Site
from twisted.web.static import File, Data
from twisted.web.util import redirectTo, Redirect
from twisted.web.resource import Resource
from twisted.python.filepath import FilePath

from lae_site.handlers.web import JinjaHandler
from lae_site.handlers.submit_subscription import SubmitSubscriptionHandler

from lae_site import __file__ as _lae_root

_STATIC = FilePath(_lae_root).sibling("static")

def configuration(stripe_publishable_api_key):
    """
    Create a ``Resource`` which serves up simple configuration used by
    JavaScript on the website.
    """
    return Data(
        dumps({
            # Stripe publishable key identifies a Stripe account in
            # API uses.  It's safe to share and required by the
            # JavaScript Stripe client API.
            u"stripe-publishable-api-key": stripe_publishable_api_key,
        }),
        b"application/json",
    )



def make_resource(
        stripe_publishable_api_key,
        signup, stripe, mailer,
):
    resource = Resource()
    resource.putChild("", Redirect("https://leastauthority.com/"))
    resource.putChild("index.html", Redirect("https://leastauthority.com/"))
    resource.putChild('signup', Redirect("https://leastauthority.com/"))
    resource.putChild('static', File(_STATIC.path))
    resource.putChild(
        'configuration',
        configuration(stripe_publishable_api_key),
    )
    resource.putChild(
        "s4-subscription-form",
        JinjaHandler("s4-subscription-form.html"),
    )
    resource.putChild(
        'submit-subscription',
        SubmitSubscriptionHandler(
            signup, mailer, stripe,
        ),
    )

    return resource



def make_site(resource, site_logs_path):
    site = Site(resource, logPath=site_logs_path.path)
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
        return redirectTo(bytes(newpath), request)

    def getChild(self, name, request):
        return self


def make_redirector_site(port):
    site = Site(RedirectToHTTPS(port))
    site.displayTracebacks = False
    return site
