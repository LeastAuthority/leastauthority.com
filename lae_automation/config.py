# Copyright Least Authority Enterprises.
# See LICENSE for details.

import simplejson

from twisted.python.filepath import FilePath


class Config(object):
    # .../leastauthority.com/lae_automation/config.py -> .../k8s_secrets
    _secrets = FilePath(__file__).parent().parent().parent().child('k8s_secrets')
    DEFAULT_CONFIG_PATH = _secrets.child('config-lae-automation.json').path

    def __init__(self, configFile=None):
        """
        Given a path string or file-like object load a configuration.

        The configuration is available as attributes.
        """
        if configFile is None:
            configFile = self.DEFAULT_CONFIG_PATH

        config = self._load_config_json(configFile)
        assert isinstance(config, dict)
        self.other = config

    @staticmethod
    def _load_config_json(configFile):
        if isinstance(configFile, (str, unicode)):
            configFile = open(configFile, 'r')

        try:
            fpos = configFile.tell()
            return simplejson.load(configFile)
        except simplejson.decoder.JSONDecodeError, e:
            configFile.seek(fpos)
            data = configFile.read()
            e.args = tuple(e.args + (configFile, repr(data),))
            raise
        finally:
            configFile.close()
