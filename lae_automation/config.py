#! /usr/bin/env python

import os, simplejson


class Config(object):
    __slots__ = ['products', 'other']

    DEFAULT_CONFIG_PATH = os.path.expanduser('../lae_automation_config.json')

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
            for field in ("full_name", "product_code", "product_token", "ami_image_id", "instance_size"):
                assert field in value, value
                value[field] = str(value[field])
                assert isinstance(value[field], str), value

        self.other = config

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return simplejson.load(configFile)
        finally:
            configFile.close()
