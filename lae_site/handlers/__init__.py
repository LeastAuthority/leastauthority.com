#! /usr/bin/env python

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import redirectTo
from twisted.web.resource import Resource

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler, ActivationRequestHandler
from lae_site.handlers.signup import SignupHandler
from lae_site.handlers.web import JinjaHandler


PAGES = ('about_us',
         'support',
         'downloads',
         'design',
         'security',
         'products',
         'sign_up_info')

def make_site(config):
    resource = JinjaHandler('index.html')
    resource.putChild('static', File('content/static'))

    resource.putChild('signup', SignupHandler(config.products))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())
    resource.putChild('activation-request', ActivationRequestHandler())

    for child in PAGES:
        resource.putChild(child, JinjaHandler(child + '.html'))

    return Site(resource, logPath="sitelogs")


class RedirectToHTTPS(Resource):
    """
    I redirect to the same path at https: and remove any leading "www.".
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
        if host.startswith('www.'):
            host = host[len('www.'):]
        if self.port == 443:
            newpath.netloc = host
        else:
            newpath.netloc = "%s:%d" % (host, self.port)
        return redirectTo(newpath, request)

    def getChild(self, name, request):
        return self


def make_redirector_site(port):
    return Site(RedirectToHTTPS(port))
