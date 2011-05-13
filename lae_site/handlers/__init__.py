#! /usr/bin/env python

import logging

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import Redirect
from twisted.internet import reactor

from lae_site.handlers.devpay_complete import DevPayPurchaseHandler


def startServer(config):
    resource = File('content')

    resource.putChild('signup', Redirect( config.devpay_purchase_url ))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())

    site = Site(resource)

    logging.info('Listening on port 80...')
    reactor.listenTCP(80, site)
