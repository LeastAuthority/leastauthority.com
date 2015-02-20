
from twisted.internet import defer

from lae_util.send_email import send_plain_email, FROM_EMAIL, FROM_ADDRESS, PGP_NOTIFICATION_EMAIL


CONFIRMATION_EMAIL_SUBJECT = "Your Simple Secure Storage Service is Activated"

CONFIRMATION_EMAIL_BODY = """Hello S4 Subscriber,

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

For User support, visit

  https://leastauthority.com/support

The service is priced at $25/month for unlimited storage for personal use.
It comes with a 30-day free trial, so you can test it out with no obligation.
You may cancel at any time by sending email to <support@LeastAuthority.com>.

We hope you enjoy using our service and find it useful.

--\x20
The Least Authority Enterprises team
"""

NOTIFY_FAILURE_SUBJECT = "A sign-up failed"

NOTIFY_FAILURE_BODY = """Hello, hard-working support person.

The sign-up process failed for customer <%(customer_email)s>.
The log filename is '%(logfilename)s'.

I'm sure you'll be able to fix it in no time with your human intellect.
Good luck!

REMEMBER: the customer is NOT cc:d on this ticket by default.

--\x20
signup.py
"""


NOTIFY_FAILURE_EMAIL = "support@leastauthority.com"


def send_signup_confirmation(publichost, customer_email, external_introducer_furl, customer_keyinfo, stdout, stderr):
    # TODO: the name is URL-escaped UTF-8. It should be OK to unescape it since the email is plain text,
    # but I'm being cautious for now since I haven't reviewed email.mime.text.MIMEText to make sure that's safe.
    content = CONFIRMATION_EMAIL_BODY % {
               "external_introducer_furl": external_introducer_furl,
               "publichost": publichost
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": CONFIRMATION_EMAIL_SUBJECT,
              }

    d = defer.succeed(None)
    if customer_keyinfo:
        print >>stdout, "Notifying one of our staff to send the confirmation e-mail to <%s>..." % (customer_email,)
        # Don't send the introducer furl; they're supposed to log in to the automation server to get it.
        headers["Subject"] = "Sign-up with PGP key"
        d.addCallback(lambda ign:
                      send_plain_email(FROM_EMAIL, PGP_NOTIFICATION_EMAIL,
                                       "Please send a confirmation e-mail to %r." % (customer_email),
                                       headers))
    else:
        print >>stdout, "Sending confirmation e-mail to <%s>..." % (customer_email,)
        d.addCallback(lambda ign:
                      send_plain_email(FROM_EMAIL, customer_email, content, headers))

    def _sent(ign):
        if customer_keyinfo:
            print >>stdout, "Notification sent."
        else:
            print >>stdout, "Confirmation e-mail sent."
    def _error(f):
        print >>stdout, "Sending of e-mail failed."
        print >>stdout, "Please contact <support@leastauthority.com> to make sure that we have your correct e-mail address."
        print >>stderr, str(f)
        return f
    d.addCallbacks(_sent, _error)
    return d


def send_notify_failure(f, customer_email, logfilename, stdout, stderr):
    print >>stderr, str(f)

    content = NOTIFY_FAILURE_BODY % {
               "customer_email": customer_email,
               "logfilename": logfilename,
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": NOTIFY_FAILURE_SUBJECT,
              }

    d = send_plain_email(FROM_EMAIL, NOTIFY_FAILURE_EMAIL, content, headers)

    def _sent(ign):
        print >>stdout, "Failure notification email sent to %s." % (NOTIFY_FAILURE_EMAIL,)
    def _error(emailf):
        print >>stdout, "A failure notification could not be sent."
        print >>stderr, str(emailf)
    d.addCallbacks(_sent, _error)
    # return the original failure
    d.addBoth(lambda ign: f)
    return d
