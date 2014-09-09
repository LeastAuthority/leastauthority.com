import mock

from twisted.trial.unittest import TestCase

class CommonTestFixture(TestCase):

    def setUp(self):
        self.STUB = mock.Mock()

class TestExtractors(CommonTestFixture):

    def test_extract_logs_from_tarball(self):
        pass

    def test_extract_PGP_key(self):
        pass

    def test_extract_furl(self):
        pass

class TestPGPCall(CommonTestFixture):

    def test_import_PGP_key(self):
        pass

    def test_encrypt_sign_confirmation(self):
        pass

class TestConfirmationCreator(CommonTestFixture):

    def test_create_confirmation_email(self):
        pass
