#! /usr/bin/env python

import os
import sys
import json
import logging
import pprint

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.resource import Resource
from twisted.web.util import Redirect
from twisted.internet import reactor

def main():
    config = Config()

    logging.basicConfig(
        stream = sys.stdout,
        level = logging.DEBUG,
        format = '%(asctime)s %(levelname) 5s [%(module)s L%(lineno)d] %(message)s',
        datefmt = '%Y-%m-%dT%H:%M:%S%z')

    resource = File('content')
    resource.putChild('signup', Redirect( config.purchase_url ))
    resource.putChild('devpay-complete', DevPayPurchaseHandler())

    factory = Site(resource)
    logging.info('Listening on port 80...')
    reactor.listenTCP(80, factory)
    reactor.run()


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


class Config (object):
    def __init__(self):
        d = self._load_config_json()
        self.purchase_url = str( d.pop('purchase_url') )
        if d:
            print 'Warning: Unknown config items: %r' % (d.keys(),)

    @staticmethod
    def _load_config_json():
        path = os.path.expanduser('~/lae_website_config.json')
        f = open(path, 'r')
        try:
            return json.load(f)
        finally:
            f.close()


if __name__ == '__main__':
    main()
