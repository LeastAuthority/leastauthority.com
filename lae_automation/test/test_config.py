
from cStringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_automation.config import Config



# Test vectors:
from lae_automation.test.test_vectors import MOCK_VALID_PRODUCTS, MOCK_VALID_PLAN_ID, MOCKJSONCONFIGFILE

VALID_PRODUCTS = MOCK_VALID_PRODUCTS

VALID_CONFIG = MOCKJSONCONFIGFILE

UNKNOWN_OPTION_CONFIG = """
{
  "unknown": "xxx",
%s
}
""" % (VALID_PRODUCTS,)

INVALID_EMPTY_CONFIG = "{}"

INVALID_WRONG_KEY_CONFIG = """
{
    "ropducts": []
}
"""

class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG )

        self._assert_valid_config(config)


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


    def _assert_valid_config(self, config):
        self.failUnlessEqual(MOCK_VALID_PLAN_ID, config.products[0]["plan_ID"])


    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )
