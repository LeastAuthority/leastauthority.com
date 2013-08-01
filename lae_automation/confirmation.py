
from twisted.internet import defer
from twisted.python.filepath import FilePath

from lae_util.send_email import send_plain_email, \
     SENDER_DOMAIN, FROM_EMAIL, FROM_ADDRESS, USER_AGENT, SMTP_HOST, SMTP_PORT, SMTP_USERNAME


CONFIRMATION_EMAIL_SUBJECT = "Your sign-up to Tahoe-LAFS-on-S3 is complete"

CONFIRMATION_EMAIL_BODY = """Hello %(customer_name)s,

Congratulations on activating your Simple Secure Storage Service!  Please go to

  https://leastauthority.com/howtoconfigure

for instructions on how to set up and configure your Tahoe-LAFS gateway
and start using the service.

The settings for your gateway's tahoe.cfg are:


[client]
introducer.furl = %(external_introducer_furl)s

shares.needed = 1
shares.happy = 1
shares.total = 1


(The introducer.furl setting should be on one line.)

For more User support, visit

  https://leastauthority.zendesk.com/home

We hope you enjoy using our service and find it useful.

--\x20
The Least Authority Enterprises team
"""

NOTIFY_FAILURE_SUBJECT = "A sign-up failed"

NOTIFY_FAILURE_BODY = """Hello, hard-working support person.

The sign-up process failed for customer %(customer_name)s <%(customer_email)s>.
The log filename is '%(logfilename)s'.

I'm sure you'll be able to fix it in no time with your human intellect.
Good luck!

--\x20
signup.py
"""


NOTIFY_FAILURE_EMAIL = "support@leastauthority.com"

PGP_NOTIFICATION_EMAIL = "zancas@leastauthority.com"


def send_signup_confirmation(publichost, customer_name, customer_email, external_introducer_furl, customer_keyinfo, stdout, stderr, password_path='../secret_config/smtppassword'):
    password = FilePath(password_path).getContent().strip()

    # TODO: the name is URL-escaped UTF-8. It should be OK to unescape it since the email is plain text,
    # but I'm being cautious for now since I haven't reviewed email.mime.text.MIMEText to make sure that's safe.
    content = CONFIRMATION_EMAIL_BODY % {
               "customer_name": customer_name,
               "external_introducer_furl": external_introducer_furl,
               "publichost": publichost
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": CONFIRMATION_EMAIL_SUBJECT,
               "User-Agent": USER_AGENT,
               "Content-Type": 'text/plain; charset="utf-8"',
              }

    d = defer.succeed(None)
    if customer_keyinfo:
        print >>stdout, "Notifying one of our staff to send the confirmation e-mail to <%s>..." % (customer_email,)
        # Don't send the introducer furl; they're supposed to log in to the automation server to get it.
        headers["Subject"] = "Sign-up with PGP key"
        d.addCallback(lambda ign:
                      send_plain_email(SMTP_HOST, SMTP_USERNAME, password, FROM_EMAIL, PGP_NOTIFICATION_EMAIL,
                                       "Please send a confirmation e-mail to %r at %r." % (customer_name, customer_email),
                                       headers, SENDER_DOMAIN, SMTP_PORT))
    else:
        print >>stdout, "Sending confirmation e-mail to <%s>..." % (customer_email,)
        d.addCallback(lambda ign:
                      send_plain_email(SMTP_HOST, SMTP_USERNAME, password, FROM_EMAIL, customer_email,
                                       content, headers, SENDER_DOMAIN, SMTP_PORT))

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


def send_notify_failure(f, customer_name, customer_email, logfilename, stdout, stderr, password_path='../secret_config/smtppassword'):
    password = FilePath(password_path).getContent().strip()

    print >>stderr, str(f)

    content = NOTIFY_FAILURE_BODY % {
               "customer_name": customer_name,
               "customer_email": customer_email,
               "logfilename": logfilename,
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": NOTIFY_FAILURE_SUBJECT,
               "User-Agent": USER_AGENT,
               "Content-Type": 'text/plain; charset="utf-8"',
              }

    d = send_plain_email(SMTP_HOST, SMTP_USERNAME, password, FROM_EMAIL, NOTIFY_FAILURE_EMAIL,
                         content, headers, SENDER_DOMAIN, SMTP_PORT)

    def _sent(ign):
        print >>stdout, "Failure notification sent for the following error:"
    def _error(emailf):
        print >>stdout, "A failure notification could not be sent."
        print >>stdout, "Contacting <%s> yourself may help to resolve this problem more quickly:" % (NOTIFY_FAILURE_EMAIL,)
        print >>stderr, str(emailf)
    d.addCallbacks(_sent, _error)
    # return the original failure
    d.addBoth(lambda ign: f)
    return d
