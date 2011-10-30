#! /usr/bin/env python

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import Redirect

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler, ActivationRequestHandler


def make_site(config):
    resource = File('content')

    resource.putChild('signup', Redirect( config.devpay_purchase_url ))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())
    resource.putChild('activation-request', ActivationRequestHandler())

    return Site(resource, logPath="sitelogs")
