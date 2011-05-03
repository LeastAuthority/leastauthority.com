#! /usr/bin/env python

import logging
import pprint

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.resource import Resource
from twisted.web.util import Redirect
from twisted.internet import reactor


def startServer(config):
    resource = File('content')

    resource.putChild('signup', Redirect( config.purchase_url ))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())

    site = Site(resource)

    logging.info('Listening on port 80...')
    reactor.listenTCP(80, site)



class DevPayPurchaseHandler (Resource):
    def __init__(self, *a, **kw):
        Resource.__init__(self, *a, **kw)
        self._log = logging.getLogger(self.__class__.__name__)
        self._log.debug('Initialized.')

    def render(self, request):
        details = dict(
            [ (k, getattr(request, k))
              for k in ['method',
                        'uri',
                        'path',
                        'args',
                        'received_headers']
              ])
        details['client-ip'] = request.getClientIP()
        self._log.debug('Request details from %r:\n%s', request, pprint.pformat(details))
        request.setResponseCode(200)
        request.setHeader('content-type', 'text/ascii')
        return pprint.pformat(details)
