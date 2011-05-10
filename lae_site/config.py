#! /usr/bin/env python

import os
import json


from txaws.credentials import AWSCredentials


class MissingConfiguration (Exception):
    pass


class Config (object):

    __slots__ = ['aws_creds', 'devpay_purchase_url', 'unknown_options']

    DEFAULT_CONFIG_PATH = os.path.expanduser('~/lae_website_config.json')

    def __init__(self, configFile = DEFAULT_CONFIG_PATH):
        """
        Given a path string or file-like object load a configuration.

        The configuration is available as attributes.
        """
        d = self._load_config_json(configFile)

        def pop_required(name):
            try:
                return str( d.pop(name) )
            except KeyError:
                raise MissingConfiguration(name)

        self.devpay_purchase_url = pop_required('devpay_purchase_url')

        accesskey = pop_required('aws_access_key')
        secretkey = pop_required('aws_secret_key')

        self.aws_creds = AWSCredentials(
            access_key = accesskey,
            secret_key = secretkey,
            )

        self.unknown_options = d

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return json.load( configFile )
        finally:
            configFile.close()
