import pprint
from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config, MissingConfiguration


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG )

        self.assertEqual( VALID_PURCHASE_URL, config.purchase_url )
        

    def test_missing_required_key(self):
        for vector in [INVALID_EMPTY_CONFIG, INVALID_WRONG_KEY_CONFIG]:
            try:
                config = self._load_from_string( vector )
            except MissingConfiguration, e:
                expected = ('purchase_url',)
                self.assertEqual( expected, e.args )
            else:
                self.fail(
                    'Incorrectly loaded:\n-Invalid Configuration-\n%s-Result-\n%s',
                    vector,
                    pprint.pformat(config),
                    )

    def test_unknown_options(self):
        config = self._load_from_string( UNKNOWN_OPTION_CONFIG )

        expected = { UNKNOWN_OPTION_NAME: UNKNOWN_OPTION_VALUE }

        self.assertEqual(expected, config.unknown_options)
    
    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )


# Test vectors:
VALID_PURCHASE_URL = 'http://fakey-site.crom/fnorp?id=1234'

VALID_CONFIG = """
{
    "purchase_url": "%s"
}
""" % (VALID_PURCHASE_URL,)

UNKNOWN_OPTION_NAME = 'thingy'
UNKNOWN_OPTION_VALUE = True

UNKNOWN_OPTION_CONFIG = """
{
    "purchase_url": "%s",
    "%s": true
}
""" % (VALID_PURCHASE_URL, UNKNOWN_OPTION_NAME)

INVALID_EMPTY_CONFIG = '{}'

INVALID_WRONG_KEY_CONFIG = """
{
    "pruchase_rul": "%s"
}
""" % (VALID_PURCHASE_URL,)

