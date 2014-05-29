
from cStringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string(EMPTY_CONFIG)
        self.failIf(config.other)

    def test_other(self):
        config = self._load_from_string(UNKNOWN_OPTION_CONFIG)
        self.failUnlessEqual("xxx", config.other["unknown"])

    def test_load_file(self):
        temp = self.mktemp()
        with file(temp, 'w') as f:
            f.write(UNKNOWN_OPTION_CONFIG)

        config = Config(temp)
        self.failUnlessEqual("xxx", config.other["unknown"])


    @staticmethod
    def _load_from_string(text):
        return Config(StringIO(text))


# Test vectors:

EMPTY_CONFIG = """
{
}
"""

UNKNOWN_OPTION_CONFIG = """
{
  "unknown": "xxx"
}
"""
