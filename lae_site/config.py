#! /usr/bin/env python

import os
import json


class MissingConfiguration (Exception):
    pass


class Config (object):

    __slots__ = ['purchase_url', 'unknown_options']

    DEFAULT_CONFIG_PATH = os.path.expanduser('~/lae_website_config.json')

    def __init__(self, configFile = DEFAULT_CONFIG_PATH):
        """
        Given a path string or file-like object load a configuration.

        The configuration is available as attributes.
        """
        d = self._load_config_json(configFile)

        try:
            self.purchase_url = str( d.pop('purchase_url') )
        except KeyError:
            raise MissingConfiguration('purchase_url')

        self.unknown_options = d

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return json.load( configFile )
        finally:
            configFile.close()
