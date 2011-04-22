#! /usr/bin/env python

import os
import json

from twisted.web.server import Site
from twisted.web.static import File
from twisted.web.util import Redirect
from twisted.internet import reactor

def main():
    config = Config()

    resource = File('content')
    resource.putChild('signup', Redirect( config.purchase_url ))

    factory = Site(resource)
    reactor.listenTCP(80, factory)
    reactor.run()


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
