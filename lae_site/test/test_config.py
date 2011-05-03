from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config


VALID_PURCHASE_URL = "http://fakey-site.crom/fnorp?id=1234"

VALID_CONFIG_TEXT = """
{
    "purchase_url": "%s"
}
""" % (VALID_PURCHASE_URL,)


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG_TEXT )

        self.assertEqual( VALID_PURCHASE_URL, config.purchase_url )
        
    
    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )
