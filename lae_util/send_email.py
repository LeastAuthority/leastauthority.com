
from cStringIO import StringIO
from email.mime.text import MIMEText

from twisted.internet import defer
from twisted.internet.reactor import connectTCP
from twisted.mail.smtp import messageid, rfc822date, ESMTPSenderFactory


SENDER_DOMAIN = "leastauthority.com"
FROM_EMAIL = "info@leastauthority.com"
FROM_ADDRESS = "Least Authority <%s>" % (FROM_EMAIL,)
SUPPORT_ADDRESS = "Least Authority Support <support@leastauthority.com>"
USER_AGENT = "Least Authority e-mail sender"

SMTP_HOST = "smtp.googlemail.com"
SMTP_PORT = 25
SMTP_USERNAME = FROM_EMAIL


def send_plain_email(smtphost, username, password, fromEmail, toEmail, content, headers,
                     senderDomainName=None, port=25, requireSSL=True):
    requireAuth = bool(password)
    msg = MIMEText(content)

    # Setup the mail headers
    for (header, value) in headers.items():
        msg[header] = value

    headkeys = [k.lower() for k in headers.keys()]

    # Add required headers if not present
    if "message-id" not in headkeys:
        msg["Message-ID"] = messageid()
    if "date" not in headkeys:
        msg["Date"] = rfc822date()
    if "from" not in headkeys and "sender" not in headkeys:
        msg["From"] = fromEmail
    if "to" not in headkeys and "cc" not in headkeys and "bcc" not in headkeys:
        msg["To"] = toEmail

    # send message
    f = StringIO(msg.as_string())
    d = defer.Deferred()
    factory = ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d,
                                 requireAuthentication=requireAuth,
                                 requireTransportSecurity=requireSSL)
    if senderDomainName is not None:
        factory.domain = senderDomainName
    connectTCP(smtphost, port, factory)

    return d
