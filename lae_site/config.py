
import simplejson


class Config(object):
    DEFAULT_CONFIG_PATH = '../secret_config/lae_site_config.json'

    def __init__(self, configFile = DEFAULT_CONFIG_PATH):
        """
        Given a path string or file-like object load a configuration.

        The configuration is available as attributes.
        """
        config = self._load_config_json(configFile)

        assert isinstance(config, dict)
        self.other = config

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return simplejson.load(configFile)
        finally:
            configFile.close()
