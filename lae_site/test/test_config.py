
from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG )

        self._assert_valid_config(config)


    def test_missing_required_key(self):
        for vector in [INVALID_EMPTY_CONFIG, INVALID_WRONG_KEY_CONFIG]:
            self.failUnlessRaises(AssertionError, self._load_from_string, vector)


    def test_unknown_options(self):
        config = self._load_from_string( UNKNOWN_OPTION_CONFIG )

        self.failUnlessEqual("xxx", config.unknown_options["unknown"])


    def test_load_file(self):
        temp = self.mktemp()

        with file(temp, 'w') as f:
            f.write(VALID_CONFIG)

        self._assert_valid_config(Config(temp))


    def _assert_valid_config(self, config):
        self.failUnlessEqual(VALID_SIGNUP_URL, config.products[0]["signup_url"])
        self.failUnlessEqual(True, config.products[0]["listed"])


    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )


# Test vectors:
VALID_SIGNUP_URL = 'http://fakey-site.crom/fnorp?id=1234'

VALID_PRODUCTS = """
  "products": [
    { "short_name":    "goodness",
      "full_name":     "Wonderous cloud storage goodness",
      "listed":        "true",
      "signup_url":    "%s",
      "product_code":  "12345678",
      "product_token": "{ProductToken}blah",
      "unknown":       "xxx"
    }
  ]
""" % (VALID_SIGNUP_URL,)

VALID_CONFIG = """
{
%s
}
""" % (VALID_PRODUCTS,)

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

