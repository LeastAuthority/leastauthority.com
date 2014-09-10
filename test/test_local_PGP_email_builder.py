import mock

from twisted.trial.unittest import TestCase

import local_PGP_email_builder


class CommonTestFixture(TestCase):
    def setUp(self):
        self.MOCK_SP_CALL = mock.Mock()
        def call_subprocess_call(call_list):
            self.MOCK_SP_CALL(call_list)


        self.GLOB_GLOB_CALL = mock.Mock()
        def call_glob_glob(call_list):
            self.MOCK_SP_CALL(call_list)


    def test_extract_logs_from_tarball(self):
        local_PGP_email_builder.extract_logs_from_tarball("FOO@SPAM.tar.bz2", "FAKESERVICEID")
