#! /usr/bin/env python

import os, json


class Config (object):

    __slots__ = ['products', 'unknown_options']

    DEFAULT_CONFIG_PATH = os.path.expanduser('../lae_website_config.json')

    def __init__(self, configFile = DEFAULT_CONFIG_PATH):
        """
        Given a path string or file-like object load a configuration.

        The configuration is available as attributes.
        """
        config = self._load_config_json(configFile)

        assert isinstance(config, dict)
        assert "products" in config, config
        self.products = config.pop("products")
        assert isinstance(self.products, list)
        for value in self.products:
            assert isinstance(value, dict), value
            assert "short_name" in value, value
            assert "full_name" in value, value
            assert "signup_url" in value, value
            assert "product_code" in value, value
            assert "product_token" in value, value
            assert isinstance(value["short_name"], basestring), value
            assert isinstance(value["full_name"], basestring), value
            assert isinstance(value["signup_url"], basestring), value
            assert isinstance(value["product_code"], basestring), value
            assert isinstance(value["product_token"], basestring), value

        self.unknown_options = config

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return json.load( configFile )
        finally:
            configFile.close()
