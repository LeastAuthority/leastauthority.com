from simplejson.scanner import JSONDecodeError

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_automation import pgp

from lae_automation.test.test_vectors import MOCKCORRECTSSEC2CONFIGFILEJSON
from lae_automation.test.test_vectors import MOCKNOTASCIISSEC2CONFIGFILEJSON

class CommonTestFixture(TestCase):

    def setUp(self):
        self.SSEC2CONFIGFILEPATH = 'SSEC2'

class TestExtractPGPKey(CommonTestFixture):

    def test_extract_valid_PGP_key(self):
        pass

class TestExtractFurl(CommonTestFixture):

    def test_extract_valid_furl(self):
        FilePath(self.SSEC2CONFIGFILEPATH).setContent(MOCKCORRECTSSEC2CONFIGFILEJSON)
        extracted_furl = pgp.extract_furl()
        self.failUnlessEqual(extracted_furl, "pb://bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb@54.164.165.217:12345/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    def test_extract_non_ascii_furl(self):
        FilePath(self.SSEC2CONFIGFILEPATH).setContent(MOCKNOTASCIISSEC2CONFIGFILEJSON)
        self.failUnlessRaises(JSONDecodeError, pgp.extract_furl)

class TestImportPGPKey(CommonTestFixture):

    def test_import_valid_PGP_key(self):
        pass

class TestEncryptSignConfirmation(CommonTestFixture):

    def test_valid_encryption_and_signing_keys(self):
        pass

class TestCreateConfirmationEmail(CommonTestFixture):

    def test_valid_email_body(self):
        pass
