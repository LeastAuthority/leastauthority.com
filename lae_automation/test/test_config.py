
from cStringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_automation.config import Config



# Test vectors:
from lae_automation.test.test_vectors import MOCKJSONCONFIGFILE

VALID_CONFIG = MOCKJSONCONFIGFILE

UNKNOWN_OPTION_CONFIG = """
{
  "unknown": "xxx"
}
"""

INVALID_EMPTY_CONFIG = "{}"

INVALID_WRONG_KEY_CONFIG = """
{
    "ropducts": []
}
"""

class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        self._load_from_string( VALID_CONFIG )


    def test_missing_required_key(self):
        for vector in [INVALID_EMPTY_CONFIG, INVALID_WRONG_KEY_CONFIG]:
            self.failUnlessRaises(AssertionError, self._load_from_string, vector)


    def test_other(self):
        config = self._load_from_string( UNKNOWN_OPTION_CONFIG )

        self.failUnlessEqual("xxx", config.other["unknown"])


    def test_load_file(self):
        temp = self.mktemp()

        with file(temp, 'w') as f:
            f.write(VALID_CONFIG)

        self._assert_valid_config(Config(temp))


    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )
