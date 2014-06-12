
from mock import MagicMock, patch
from twisted.trial.unittest import TestCase

from lae_automation.confirmation import CONFIRMATION_EMAIL_SUBJECT
from lae_util.send_email import compose_plain_email, MIMEText, FROM_EMAIL


class EmailSendingCommonFixture(TestCase):
    def setUp(self):
        self.msg_MOCK_patcher = patch('lae_util.send_email.MIMEText', spec=True)
        self.msg_MOCK_class = self.msg_MOCK_patcher.start()
        self.msg_MOCK_class.as_string.return_value = 'SOMESTRING'

    def tearDown(self):
        self.msg_MOCK = self.msg_MOCK_patcher.stop()


class PGPLessConfirmation(EmailSendingCommonFixture):
    def setUp(self):
        EmailSendingCommonFixture.setUp(self)
        self.headers_MOCK = { "From": FROM_EMAIL, "Subject": CONFIRMATION_EMAIL_SUBJECT }

    def tearDown(self):
        super(EmailSendingCommonFixture, self).tearDown()

    def test_compose_email(self):
        compose_plain_email( MagicMock(name='fromEmail'),
                             MagicMock(name='toEmail'),
                             MagicMock(name='content'),
                             self.headers_MOCK )
