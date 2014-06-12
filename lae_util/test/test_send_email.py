
from twisted.trial.unittest import TestCase

from lae_automation.confirmation import CONFIRMATION_EMAIL_SUBJECT
from lae_util.send_email import compose_plain_email, FROM_EMAIL, FROM_ADDRESS


class TestEmailComposition(TestCase):
    def test_compose_pgpless_email(self):
        TEST_HEADERS = {"From": FROM_ADDRESS, "Subject": CONFIRMATION_EMAIL_SUBJECT}
        result = compose_plain_email( FROM_EMAIL,
                                      "test@test",
                                      'content',
                                      TEST_HEADERS )
        self.failUnlessEqual(result.count("Content-Type"), 1)
