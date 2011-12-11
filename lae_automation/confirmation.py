
from cStringIO import StringIO
from email.mime.text import MIMEText

from twisted.internet import defer, reactor
from twisted.python.filepath import FilePath
from twisted.mail import smtp


CONFIRMATION_EMAIL_SUBJECT = "Your sign-up to Tahoe-LAFS-on-S3 is complete"

CONFIRMATION_EMAIL_BODY = """Hello %(customer_name)s,

Welcome to the alpha programme!  Please go to

  https://leastauthority.com/howtoconfigure

for instructions on how to set up and configure your Tahoe-LAFS gateway
and start using the service.

The settings for your gateway's tahoe.cfg are:


[client]
introducer.furl = %(external_introducer_furl)s

shares.needed = 1
shares.happy = 1
shares.total = 1

[storage]
enabled = false


(The introducer.furl setting should be on one line.)

We hope you enjoy using our service and find it useful.

--\x20
The Least Authority Enterprises team
"""

SENDER_DOMAIN = "leastauthority.com"
FROM_EMAIL = "davidsarah@leastauthority.com"
FROM_ADDRESS = "Least Authority Enterprises <%s>" % (FROM_EMAIL,)
USER_AGENT = "Least Authority Enterprises e-mail sender"

SMTP_HOST = "smtp.googlemail.com"
SMTP_USERNAME = FROM_EMAIL

PGP_NOTIFICATION_EMAIL = "davidsarah@leastauthority.com"


def send_signup_confirmation(customer_name, customer_email, external_introducer_furl, customer_keyinfo, stdout, stderr):
    password = FilePath('../smtppassword').getContent().strip()

    content = CONFIRMATION_EMAIL_BODY % {
               "customer_name": customer_name,
               "external_introducer_furl": external_introducer_furl,
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": CONFIRMATION_EMAIL_SUBJECT,
               "User-Agent": USER_AGENT,
              }

    if customer_keyinfo:
        print >>stdout, "Notifying one of our staff to send the confirmation e-mail to <%s>..." % (customer_email,)
        # Don't send the introducer furl; they're supposed to log in to the automation server to get it.
        headers["Subject"] = "Sign-up with PGP key"
        d = send_plain_email(SMTP_HOST, SMTP_USERNAME, password,
                             FROM_EMAIL, PGP_NOTIFICATION_EMAIL,
                             "Please send a confirmation e-mail to %r at %r." % (customer_name, customer_email), headers, SENDER_DOMAIN)
    else:
        print >>stdout, "Sending confirmation e-mail to <%s>..." % (customer_email,)
        d = send_plain_email(SMTP_HOST, SMTP_USERNAME, password,
                             FROM_EMAIL, customer_email, content, headers, SENDER_DOMAIN)

    def _sent(ign):
        if customer_keyinfo:
            print >>stdout, "Notification sent."
        else:
            print >>stdout, "Confirmation e-mail sent."
    def _error(f):
        print >>stdout, "Sending of e-mail failed."
        print >>stdout, "Please contact <info@leastauthority.com> to make sure that we have your correct e-mail address."
        print >>stderr, str(f)
        return f
    d.addCallbacks(_sent, _error)
    return d


def send_plain_email(smtphost, username, password, fromEmail, toEmail, content, headers, senderDomainName=None, port=25):
    msg = MIMEText(content)

    # Setup the mail headers
    for (header, value) in headers.items():
        msg[header] = value

    headkeys = [k.lower() for k in headers.keys()]

    # Add required headers if not present
    if "message-id" not in headkeys:
        msg["Message-ID"] = smtp.messageid()
    if "date" not in headkeys:
        msg["Date"] = smtp.rfc822date()
    if "from" not in headkeys and "sender" not in headkeys:
        msg["From"] = fromEmail
    if "to" not in headkeys and "cc" not in headkeys and "bcc" not in headkeys:
        msg["To"] = toEmail

    # send message
    f = StringIO(msg.as_string())
    d = defer.Deferred()
    factory = smtp.ESMTPSenderFactory(username, password, fromEmail, toEmail, f, d)
    if senderDomainName is not None:
        factory.domain = senderDomainName
    reactor.connectTCP(smtphost, port, factory)

    return d
