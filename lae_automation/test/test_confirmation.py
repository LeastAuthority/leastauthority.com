
from cStringIO import StringIO

from mock import Mock

from twisted.trial import unittest
from twisted.python.filepath import FilePath
from twisted.python.failure import Failure
from foolscap.api import eventually

from lae_automation import confirmation
from lae_automation.confirmation import send_signup_confirmation


class TestConfirmation(unittest.TestCase):
    SMTP_HOST = confirmation.SMTP_HOST
    SMTP_PORT = confirmation.SMTP_PORT
    SMTP_USERNAME = confirmation.SMTP_USERNAME
    SMTP_PASSWORD = 'supersekret'
    SENDER_DOMAIN = confirmation.SENDER_DOMAIN
    FROM_EMAIL = confirmation.FROM_EMAIL
    CUSTOMER_NAME = 'Fred Bloggs'
    CUSTOMER_EMAIL = 'fbloggs@example.net'
    EXTERNAL_INTRODUCER_FURL = 'pb://foo@bar/baz'
    CUSTOMER_KEYINFO = ''

    def setUp(self):
        FilePath('smtppassword').setContent(self.SMTP_PASSWORD)

    def test_send_signup_confirmation_success(self):
        the_factory = Mock()
        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d):
            self.failUnlessEqual(username, self.SMTP_USERNAME)
            self.failUnlessEqual(password, self.SMTP_PASSWORD)
            self.failUnlessEqual(fromEmail, self.FROM_EMAIL)
            self.failUnlessEqual(toEmail, self.CUSTOMER_EMAIL)
            f.seek(0, 0)
            # assume f can be read in one call
            message = f.read()
            assert f.read() == ''

            # although MIME specifies CRLF line endings, it is just LF at this point
            (headers, sep, body) = message.partition('\n\n')
            self.failUnlessEqual(sep, '\n\n')
            self.failUnlessIn('Message-ID: ', headers)
            self.failUnlessIn('Date: ', headers)
            self.failUnlessIn('Subject: ', headers)
            self.failUnlessIn('From: ', headers)
            self.failUnlessIn('To: ', headers)
            # FIXME: test for UTF-8
            self.failUnlessIn('Content-Type: text/plain', headers)
            self.failUnlessIn(self.CUSTOMER_NAME, body)
            self.failUnlessIn('https://leastauthority.com/howtoconfigure', body)
            self.failUnlessIn(self.EXTERNAL_INTRODUCER_FURL, body)

            eventually(d.callback, None)
            return the_factory
        self.patch(confirmation, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        connected = {}
        def call_connectTCP(smtphost, port, factory):
            self.failUnlessEqual(smtphost, self.SMTP_HOST)
            self.failUnlessEqual(port, self.SMTP_PORT)
            self.failUnlessEqual(factory, the_factory)
            self.failUnlessEqual(factory.domain, self.SENDER_DOMAIN)
            connected['flag'] = True
        self.patch(confirmation, 'connectTCP', call_connectTCP)

        stdout = StringIO()
        stderr = StringIO()
        d = send_signup_confirmation(self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, self.EXTERNAL_INTRODUCER_FURL,
                                     self.CUSTOMER_KEYINFO, stdout, stderr, password_path='smtppassword')
        def _check(ign):
            self.failUnless('flag' in connected)
            out = stdout.getvalue()
            self.failUnlessIn("Sending confirmation e-mail", out)
            self.failUnlessIn(self.CUSTOMER_EMAIL, out)
            self.failUnlessIn("Confirmation e-mail sent.", out)
        d.addCallback(_check)
        return d

    def test_send_signup_confirmation_factory_exception(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d):
            raise Exception('foo')
        self.patch(confirmation, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        d = send_signup_confirmation(self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, self.EXTERNAL_INTRODUCER_FURL,
                                     self.CUSTOMER_KEYINFO, stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(Exception)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_send_signup_confirmation_factory_failure(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d):
            eventually(d.errback, Exception('foo'))
        self.patch(confirmation, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        d = send_signup_confirmation(self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, self.EXTERNAL_INTRODUCER_FURL,
                                     self.CUSTOMER_KEYINFO, stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(Exception)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_send_signup_confirmation_connect_exception(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d):
            eventually(d.callback, None)
            return Mock()
        self.patch(confirmation, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        def call_connectTCP(smtphost, port, factory):
            raise Exception("foo")
        self.patch(confirmation, 'connectTCP', call_connectTCP)

        d = send_signup_confirmation(self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, self.EXTERNAL_INTRODUCER_FURL,
                                     self.CUSTOMER_KEYINFO, stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(Exception)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d
