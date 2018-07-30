from json import dumps
from datetime import datetime

from twisted.web.server import Site
from twisted.web.static import File, Data
from twisted.web.util import redirectTo, Redirect
from twisted.web.resource import Resource
from twisted.python.filepath import FilePath

from lae_site.handlers.web import JinjaHandler
from lae_site.handlers.create_subscription import CreateSubscription

from lae_site import __file__ as _lae_root

_STATIC = FilePath(_lae_root).sibling("static")

# Note the price here is descriptive for the user.  It does not control the
# actual amount billed by the payment processor.
_PRICE = {
    ("250GB", "monthly"): "9.95",
    ("250GB", "yearly"): "108",
    ("5TB", "monthly"): "25.95",
    ("5TB", "yearly"): "299",
}

_PLANS = list(
    (size, period, currency, _PRICE[size, period])
    for size in (u"250GB", u"5TB")
    for period in (u"monthly", u"yearly")
    for currency in (u"EUR", u"USD")
)


class _ResourceWithHeaders(Resource):
    """
    Add response headers to the behavior of another resource.
    """
    def __init__(self, headers, wrapped):
        """
        :param Headers headers: Additional headers to include in any rendered
            response.

        :param IResource wrapped: Another resource which is responsible for
            all response behavior apart from the additional headers.
        """
        self._wrapped = wrapped
        self._headers = headers
        Resource.__init__(self)


    def render(self, request):
        """
        Render the additional headers into a response and then delegate to the
        wrapped resource.
        """
        for k, vs in self._headers.getAllRawHeaders():
            for v in vs:
                request.responseHeaders.addRawHeader(k, v)
        return self._wrapped.render(request)


def _plan(**kw):
    kw[u"id"] = u"s4_{size}_{period}_{currency}".format(**kw).lower()
    return kw


def configuration(stripe_publishable_api_key, cross_domain):
    """
    Create a ``Resource`` which serves up simple configuration used by
    JavaScript on the website.
    """
    return Data(
        dumps({
            # Stripe publishable key identifies a Stripe account in API uses.
            # It's safe to share and required by the JavaScript Stripe client
            # API.
            u"stripe-publishable-api-key": stripe_publishable_api_key,
            u"cross-domain": cross_domain,
            u"plans": list(
                _plan(size=size, period=period, currency=currency, price=price)
                for (size, period, currency, price)
                in _PLANS
            ),
        }),
        b"application/json",
    )


def make_resource(
    stripe_publishable_api_key,
    get_signup,
    chargebee,
    mailer,
    cross_domain,
):
    resource = Resource()
    resource.putChild("", Redirect("https://leastauthority.com/"))
    resource.putChild("index.html", Redirect("https://leastauthority.com/"))
    resource.putChild('signup', Redirect("https://leastauthority.com/"))
    resource.putChild('static', File(_STATIC.path))
    resource.putChild(
        'configuration',
        configuration(stripe_publishable_api_key, cross_domain),
    )
    resource.putChild(
        "s4-subscription-form",
        JinjaHandler("s4-subscription-form.html"),
    )

    v2 = Resource()
    v2.putChild(
        "create-subscription",
        CreateSubscription(
            get_signup,
            mailer,
            chargebee,
            u"application/json",
        ),
    )
    resource.putChild("v2", v2)

    return resource



class _LogFormatter(object):
    def __init__(self, now):
        self.now = now


    def json_access_log(self, timestamp, request):
        # Just ignore the given timestamp.  It's in an awful format.
        timestamp = self.now()
        return dumps(dict(
            timestamp=timestamp.isoformat(),
            ip=request.getClientIP() or None,
            method=request.method,
            uri=request.uri,
            protocol=request.clientproto,
            code=request.code,
            length=request.sentLength or None,
            referrer=request.getHeader(b"referer") or None,
            agent=request.getHeader(b"user-agent") or None,
        ))



def make_site(resource, site_logs_path):
    site = Site(
        resource,
        logPath=site_logs_path.path,
        logFormatter=_LogFormatter(datetime.utcnow).json_access_log,
    )
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
    # XXX It would be good to combine this with make_site so we didn't end up
    # with http logs in multiple places.  Not sure how to do that though.
    site = Site(RedirectToHTTPS(port))
    site.displayTracebacks = False
    return site
