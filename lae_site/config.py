
import os, json


class Config(object):
    __slots__ = ['products', 'other']

    DEFAULT_CONFIG_PATH = os.path.expanduser('../lae_site_config.json')

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
            for field in ("short_name", "full_name", "listed", "signup_url", "product_code"):
                assert field in value, value
                value[field] = str(value[field])
                assert isinstance(value[field], str), value

            value["listed"] = (value["listed"].lower() in ("true", "yes"))

        self.other = config

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
            configFile = open(configFile, 'r')

        try:
            return json.load( configFile )
        finally:
            configFile.close()
