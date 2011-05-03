import pprint
from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config, MissingConfiguration


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG_TEXT )

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
    
    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )


# Test vectors:
VALID_PURCHASE_URL = 'http://fakey-site.crom/fnorp?id=1234'

VALID_CONFIG_TEXT = """
{
    "purchase_url": "%s"
}
""" % (VALID_PURCHASE_URL,)

INVALID_EMPTY_CONFIG = '{}'

INVALID_WRONG_KEY_CONFIG = """
{
    "pruchase_rul": "%s"
}
""" % (VALID_PURCHASE_URL,)


