from cStringIO import StringIO

from mock import Mock

from twisted.trial import unittest
from twisted.python.filepath import FilePath
from foolscap.api import eventually

from lae_util import send_email
from lae_automation import updatebulletins
from lae_automation.updatebulletins import send_bulletin


class MarkerException(Exception):
    pass


class TestBulletins(unittest.TestCase):
    SMTP_HOST = updatebulletins.SMTP_HOST
    SMTP_PORT = updatebulletins.SMTP_PORT
    SMTP_USERNAME = updatebulletins.SMTP_USERNAME
    SMTP_PASSWORD = 'supersekret'
    SENDER_DOMAIN = updatebulletins.SENDER_DOMAIN
    FROM_EMAIL = updatebulletins.FROM_EMAIL
    CUSTOMER_NAME = 'Fred Bloggs'
    CUSTOMER_EMAIL = 'fbloggs@example.net'
    PGP_NOTIFICATION_EMAIL = updatebulletins.PGP_NOTIFICATION_EMAIL
    PUBIP = '0.0.0.0'

    def setUp(self):
        FilePath('smtppassword').setContent(self.SMTP_PASSWORD)


    def _call_ESMTPSenderFactory_non_PGP(self, username, password, fromEmail, toEmail, f, d,
                                         retries=5, timeout=None, contextFactory=None, heloFallback=False,
                                         requireAuthentication=True, requireTransportSecurity=True):
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
        self.failUnlessIn('https://monitoring.leastauthority.com/', body)
        self.failUnlessIn('%s' % (self.PUBIP,), body)
        self.failUnlessIn('https://leastauthority.com/support', body)

        eventually(d.callback, None)
        return self.the_factory

    def _call_ESMTPSenderFactory_PGP(self, username, password, fromEmail, toEmail, f, d,
                                     retries=5, timeout=None, contextFactory=None, heloFallback=False,
                                     requireAuthentication=True, requireTransportSecurity=True):
        self.failUnlessEqual(username, self.SMTP_USERNAME)
        self.failUnlessEqual(password, self.SMTP_PASSWORD)
        self.failUnlessEqual(fromEmail, self.FROM_EMAIL)
        self.failUnlessEqual(toEmail, self.PGP_NOTIFICATION_EMAIL)
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
        #self.failUnlessIn('https://monitoring.leastauthority.com/', body)
        #self.failUnlessIn('/%s/' % (self.PUBIP,), body)
        #self.failUnlessIn('https://leastauthority.com/support', body)

        eventually(d.callback, None)
        return self.the_factory

    def _test_send_bulletin_success(self, call_factory, customer_keyinfo):
        self.the_factory = Mock()
        self.patch(send_email, 'ESMTPSenderFactory', call_factory)

        connected = {}
        def call_connectTCP(smtphost, port, factory):
            self.failUnlessEqual(smtphost, self.SMTP_HOST)
            self.failUnlessEqual(port, self.SMTP_PORT)
            self.failUnlessEqual(factory, self.the_factory)
            self.failUnlessEqual(factory.domain, self.SENDER_DOMAIN)
            connected['flag'] = True
        self.patch(send_email, 'connectTCP', call_connectTCP)

        stdout = StringIO()
        stderr = StringIO()
        d = send_bulletin(self.PUBIP, self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, customer_keyinfo, stdout, stderr, password_path='smtppassword')
        def _check(ign):
            self.failUnless('flag' in connected)
            out = stdout.getvalue()
            self.failUnlessIn("update bulletin e-mail", out)
            self.failUnlessIn(self.CUSTOMER_EMAIL, out)
            self.failUnlessIn("sent.", out)
        d.addCallback(_check)
        return d

    def test_send_bulletin_success_non_PGP(self):
        return self._test_send_bulletin_success(self._call_ESMTPSenderFactory_non_PGP, '')

    def test_send_bulletin_success_PGP(self):
        return self._test_send_bulletin_success(self._call_ESMTPSenderFactory_PGP, '1234 ... ABCD')

    def test_send_bulletin_factory_exception(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d,
                                    retries=5, timeout=None, contextFactory=None, heloFallback=False,
                                    requireAuthentication=True, requireTransportSecurity=True):
            raise MarkerException()
        self.patch(send_email, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        d = send_bulletin(self.PUBIP, self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, '', stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(MarkerException)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of bulletin e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_send_bulletin_factory_failure(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d,
                                    retries=5, timeout=None, contextFactory=None, heloFallback=False,
                                    requireAuthentication=True, requireTransportSecurity=True):
            eventually(d.errback, MarkerException())
            return Mock()
        self.patch(send_email, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        def call_connectTCP(smtphost, port, factory):
            pass
        self.patch(send_email, 'connectTCP', call_connectTCP)

        d = send_bulletin(self.PUBIP, self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, '', stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(MarkerException)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of bulletin e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_send_bulletin_connect_exception(self):
        stdout = StringIO()
        stderr = StringIO()

        def call_ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d,
                                    retries=5, timeout=None, contextFactory=None, heloFallback=False,
                                    requireAuthentication=True, requireTransportSecurity=True):
            eventually(d.callback, None)
            return Mock()
        self.patch(send_email, 'ESMTPSenderFactory', call_ESMTPSenderFactory)

        def call_connectTCP(smtphost, port, factory):
            raise MarkerException()
        self.patch(send_email, 'connectTCP', call_connectTCP)

        d = send_bulletin(self.PUBIP, self.CUSTOMER_NAME, self.CUSTOMER_EMAIL, '', stdout, stderr, password_path='smtppassword')
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(MarkerException)
            out = stdout.getvalue()
            self.failUnlessIn("Sending of bulletin e-mail failed", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d
