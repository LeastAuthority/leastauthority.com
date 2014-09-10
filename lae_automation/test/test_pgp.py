from simplejson.scanner import JSONDecodeError

from twisted.trial.unittest import TestCase
from twisted.python.filepath import FilePath

from lae_automation import pgp

from lae_automation.test.test_vectors import MOCKCORRECTSSEC2CONFIGFILEJSON
from lae_automation.test.test_vectors import MOCKNOTASCIISSEC2CONFIGFILEJSON

class CommonTestFixture(TestCase):

    def setUp(self):
        self.SSEC2CONFIGFILEPATH = 'SSEC2'

class TestExtractors(CommonTestFixture):

    def test_extract_logs_from_tarball(self):
        pass

    def test_extract_PGP_key(self):
        pass

class TestExtractFurl(CommonTestFixture):

    def test_extract_correct_furl(self):
        FilePath(self.SSEC2CONFIGFILEPATH).setContent(MOCKCORRECTSSEC2CONFIGFILEJSON)
        extracted_furl = pgp.extract_furl()
        self.failUnlessEqual(extracted_furl, "pb://bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb@54.164.165.217:12345/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

    def test_extract_non_ascii_furl(self):
        FilePath(self.SSEC2CONFIGFILEPATH).setContent(MOCKNOTASCIISSEC2CONFIGFILEJSON)
        self.failUnlessRaises(JSONDecodeError, pgp.extract_furl)

class TestPGPCall(CommonTestFixture):

    def test_import_PGP_key(self):
        pass

    def test_encrypt_sign_confirmation(self):
        pass

class TestConfirmationCreator(CommonTestFixture):

    def test_create_confirmation_email(self):
        pass
