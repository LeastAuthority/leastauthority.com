#! /usr/bin/env python

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import redirectTo
from twisted.web.resource import Resource

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler, ActivationRequestHandler
from lae_site.handlers.signup import SignupHandler

from lae_site.handlers.web import *


def make_site(config):

    resource = IndexPage()
    resource.putChild('static', File('content/static'))

    resource.putChild('signup', SignupHandler(config.products))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())
    resource.putChild('activation-request', ActivationRequestHandler())

    # Main handlers from lae_site.handlers.web
    resource.putChild('about_us', AboutPage())
    resource.putChild('support', SupportPage())
    resource.putChild('downloads', DownloadsPage())



    return Site(resource, logPath="sitelogs")


class RedirectToHTTPS(Resource):
    """
    I redirect to the same path at https:.
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
        if self.port == 443:
            newpath.netloc = host
        else:
            newpath.netloc = "%s:%d" % (host, self.port)
        return redirectTo(newpath, request)

    def getChild(self, name, request):
        return self


def make_redirector_site(port):
    return Site(RedirectToHTTPS(port))
