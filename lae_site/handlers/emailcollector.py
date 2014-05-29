
import re

from twisted.internet import defer
from twisted.web.server import NOT_DONE_YET

from lae_util.servers import append_record
from lae_util.send_email import send_plain_email, FROM_EMAIL, FROM_ADDRESS

from lae_site.handlers.web import env
from lae_site.handlers.main import HandlerBase

EMAILS_FILE = 'emails.csv'
VALID_EMAIL_RE = re.compile("[^@]+@[^@]+")
ACTIVE_PRODUCTS = set(['s4'])

AUTOREPLY_EMAIL_SUBJECT = "Thank you for your interest in %(full_product_name)s"

AUTOREPLY_EMAIL_BODY = """Hello,

We've received your request to sign up for %(full_product_name)s.
When we're ready to sign you up, we'll send another email.

If you have any questions in the meantime, please email them to
<support@LeastAuthority.com>.

Thanks for your patience.

--\x20
The Least Authority Enterprises team
"""


class CollectEmailHandler(HandlerBase):
    def __init__(self, basefp, out=None):
        HandlerBase.__init__(self, out=out)
        self._logger_helper(__name__)
        self.basefp = basefp

    def render(self, request):
        print >>self.out, "Yay, another potential customer:", request.args
        productname = self.get_arg(request, 'ProductName')

        if productname in ACTIVE_PRODUCTS:
            raise AssertionError("we shouldn't get here for an active product: %r", (productname,))

        email = self.get_arg(request, 'Email')
        productfullname = self.get_arg(request, 'ProductFullName')

        append_record(self.basefp.child(EMAILS_FILE), email, productname)

        request.setResponseCode(200)

        if email and VALID_EMAIL_RE.match(email):
            tmpl = env.get_template('valid_email.html')
            (start, _, rest) = tmpl.render(productname=productname, productfullname=productfullname, email=email).encode('utf-8').partition("MAGIC")
            request.write(start)

            d = defer.succeed(None)
            d.addCallback(lambda ign: send_autoreply(email, productfullname))
            def _sent(ign):
                request.write("We've just sent you an email to check that you can receive email from us.")
                request.write(rest)
            def _error(f):
                request.write("We weren't able to send email to the address you provided. This could be a problem on "
                              "our end, but please check the address. We've recorded the address anyway so that we can "
                              "try again manually.")
                request.write(rest)
                print >>self.out, str(f)
            d.addCallbacks(_sent, _error)
            d.addBoth(lambda ign: request.finish())
            def _err(f):
                print >>self.out, str(f)
            d.addErrback(_err)
            return NOT_DONE_YET
        else:
            tmpl = env.get_template('invalid_email.html')
            return tmpl.render(productname=productname, productfullname=productfullname).encode('utf-8')


def send_autoreply(customer_email, full_product_name):
    content = AUTOREPLY_EMAIL_BODY % {
                "full_product_name": full_product_name,
              }
    headers = {
               "From": FROM_ADDRESS,
               "Subject": AUTOREPLY_EMAIL_SUBJECT % {"full_product_name": full_product_name},
              }

    d = send_plain_email(FROM_EMAIL, customer_email, content, headers)
    return d
