import pprint
from StringIO import StringIO

from twisted.trial.unittest import TestCase

from lae_site.config import Config, MissingConfiguration


class ConfigTests (TestCase):
    def test_unexceptional_load(self):
        config = self._load_from_string( VALID_CONFIG )

        self._assert_valid_config(config)


    def test_missing_required_key(self):
        for vector in [INVALID_EMPTY_CONFIG, INVALID_WRONG_KEY_CONFIG]:
            try:
                config = self._load_from_string( vector )
            except MissingConfiguration, e:
                expected = ('devpay_purchase_url',)
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


    def test_load_file(self):
        temp = self.mktemp()

        with file(temp, 'w') as f:
            f.write(VALID_CONFIG)

        self._assert_valid_config(Config(temp))


    def _assert_valid_config(self, config):
        self.assertEqual( VALID_PURCHASE_URL, config.devpay_purchase_url )
        self.assertEqual( VALID_ACCESS_KEY, config.aws_creds.access_key )
        self.assertEqual( VALID_SECRET_KEY, config.aws_creds.secret_key )


    @staticmethod
    def _load_from_string(text):
        return Config( StringIO(text) )


# Test vectors:
VALID_PURCHASE_URL = 'http://fakey-site.crom/fnorp?id=1234'
VALID_ACCESS_KEY = '<{ FAKE ACCESS KEY }>'
VALID_SECRET_KEY = '<{ FAKE SECRET KEY }>'

VALID_CONFIG = """
{
    "devpay_purchase_url": "%s",
    "aws_access_key": "%s",
    "aws_secret_key": "%s"
}
""" % (VALID_PURCHASE_URL, VALID_ACCESS_KEY, VALID_SECRET_KEY)

UNKNOWN_OPTION_NAME = 'thingy'
UNKNOWN_OPTION_VALUE = True

UNKNOWN_OPTION_CONFIG = """
{
    "devpay_purchase_url": "%s",
    "aws_access_key": "%s",
    "aws_secret_key": "%s",
    "%s": true
}
""" % (VALID_PURCHASE_URL, VALID_ACCESS_KEY, VALID_SECRET_KEY, UNKNOWN_OPTION_NAME)

INVALID_EMPTY_CONFIG = '{}'

INVALID_WRONG_KEY_CONFIG = """
{
    "evpayday_pruchase_rul": "%s"
}
""" % (VALID_PURCHASE_URL,)

