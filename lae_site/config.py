#! /usr/bin/env python

import os
import json


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
