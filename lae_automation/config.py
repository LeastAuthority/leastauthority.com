
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
        assert "products" in config, config
        self.products = config.pop("products")
        assert isinstance(self.products, list)
        for value in self.products:
            assert isinstance(value, dict), value
            for field in ("amount", "interval", "currency", "plan_name", "plan_ID", "plan_name",
                          "plan_trial_period_days", "ami_image_id", "instance_size", "statement_description"):
                assert field in value, value
                value[field] = str(value[field])
                assert isinstance(value[field], str), value

        self.other = config

    @staticmethod
    def _load_config_json(configFile):
        if type(configFile) is str:
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
