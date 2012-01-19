
from cStringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_automation.config import Config


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
        self.failUnlessEqual(VALID_PRODUCT_TOKEN, config.products[0]["product_token"])


    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )


# Test vectors:
VALID_PRODUCT_TOKEN = '{ProductToken}blah'

VALID_PRODUCTS = """
  "products": [
    { "full_name":     "Wonderous cloud storage goodness",
      "product_code":  "12345678",
      "product_token": "%s",
      "ami_image_id":  "ami-abcd1234",
      "instance_size": "t1.micro",
      "unknown":       "xxx"
    }
  ]
""" % (VALID_PRODUCT_TOKEN,)

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

