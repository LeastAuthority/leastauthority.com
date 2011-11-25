
import logging, pprint, time, sys
from urllib import quote
from cgi import escape as htmlEscape

from twisted.web.resource import Resource
from twisted.web.server import NOT_DONE_YET

from lae_util.flapp import FlappCommand
from lae_util.timestamp import format_iso_time


# TODO: allow customizing the text per-product.
DEVPAY_RESPONSE_HAVE_CODES_HTML = """
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<title>Request activation for Tahoe-LAFS-on-S3 alpha</title>
</head>
<body style="background: #FFFFFF">
<p>
Thank you for requesting to sign up for the Tahoe-LAFS-on-S3 alpha!
</p>
<p>
Your sign up is not quite finished; to complete it and activate your account,
please tell us the name and email address you want us to use to communicate
with you. We will not resell or pass on this email address, and will only use it
in connection with your account, and to send you occasional announcements of new
products, features, offers and price changes.
</p>
<p>
If you use PGP or GPG, you could also send us your public key, so that we
can encrypt our emails to you and verify signed emails from you.
(Your fingerprint is enough if your key is available from the usual keyservers.)
This helps to protect you from phishing attempts -- although Tahoe-LAFS'
provider-independent security means that you don't need to tell us any secrets anyway!
</p>
<div style="width:45em">
<form action="/activation-request" method="post" enctype="multipart/form-data">
<table>
  <tr>
    <td style="width:25em"><label for="Name">Name you would like us to call you by: </label></td>
    <td><input type="text" id="Name" name="Name" style="width:20em"></td>
  </tr>
  <tr>
    <td style="width:25em"><label for="Email">Email address: </label></td>
    <td><input type="text" id="Email" name="Email" style="width:20em"><td>
  </tr>
  <tr>
    <td style="width:25em"><label for="ActivationKey">Activation Key: </label></td>
    <td><input type="text" id="ActivationKey" name="ActivationKey" value="%(activationkey)s" style="width:20em"></td>
  </tr>
  <tr>
    <td style="width:25em"><label for="ProductName">Product: </label></td>
    <td><input type="text" id="ProductName" name="ProductName" value="Tahoe-LAFS-on-S3 alpha" disabled style="width:20em">
        <input type="hidden" name="ProductCode" value="%(productcode)s"></td>
  </tr>
  <tr>
    <td style="width:25em"><label for="PublicKey">Your OpenPGP public key or fingerprint, optional: </label></td>
    <td><input type="text" id="PublicKey" name="PublicKey" style="width:20em; height:12ex"></td>
  </tr>
  <tr>
    <td><input type="submit" value="Request activation"></td>
  </tr>
</table>
</form>
</div>
<p>
Activation keys are valid for one hour. For the alpha test, the activation is
not fully automated, so the activation key may expire if we do not get to processing
it in time (which is more likely if it is outside business hours in the U.S.)
In that case, please wait until business hours, then go to
<a href="https://www.amazon.com/dp-activate">https://www.amazon.com/dp-activate</a>,
log in to your Amazon account, and click the Go to Application link.
</p>
<p>
If this form does not work for any reason, you can also send the same information
by email to <a href="mailto:info@leastauthority.com">&lt;info@leastauthority.com&gt</a>.
<p>
Happy Cloud Storing!
</p>
<p>
The Least Authority Enterprises team (Zooko, David-Sarah and Zancas)
</p>
<hr>
</body>
</html>
"""

# Amazon sometimes doesn't tell us the product code and/or activation key.
# In that case, the easiest way to get those codes is for the user to click on
# the 'Go to Application' link, so tell them to do that, rather than requiring
# them to paste in the code(s).
DEVPAY_RESPONSE_MISSING_CODE_HTML = """
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<title>Request activation for Tahoe-LAFS-on-S3 alpha</title>
</head>
<body style="background: #FFFFFF">
<p>
Thank you for requesting to sign up for the Tahoe-LAFS-on-S3 alpha!
</p>
<p>
Amazon Payments has not yet told us the activation key needed to activate your
subscription. To obtain this key, go to
<a href="https://www.amazon.com/dp-activate">https://www.amazon.com/dp-activate</a>,
log in to your Amazon account, and click the Go to Application link for this
subscription.
</p>
<p>
If you have any problem completing the sign-up, please contact
<a href="mailto:info@leastauthority.com">&lt;info@leastauthority.com&gt</a>.
<p>
Happy Cloud Storing!
</p>
<p>
The Least Authority Enterprises team (Zooko, David-Sarah and Zancas)
</p>
<hr>
</body>
</html>
"""

ACTIVATIONREQ_RESPONSE_HTML = """
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<title>Activation requested</title>
</head>
<body style="background: #FFFFFF">
<p>
Your activation request has been received.
</p>
<p>
When your account is activated, we will send you a confirmation email with
instructions on how to start using Tahoe-LAFS-on-S3, or alternatively an email
requesting any information that was missing. If you don't receive this within
an hour (also check your spam folder in case it is there), please contact
<a href="mailto:info@leastauthority.com">&lt;info@leastauthority.com&gt</a>.
</p>
<p>
Thanks again for signing up, and especially with your help for the alpha test.
</p>
<p>
The Least Authority Enterprises team (Zooko, David-Sarah and Zancas)
</p>
<hr>
<p>
The progress of your sign-up is shown below.
</p>
<pre>
"""

SUCCEEDED_HTML = """</pre>
<p>
Your activation is complete. You will shortly get a confirmation email at the address
you provided.
</p>
<hr>
</body>
</html>
"""

FAILED_HTML = """</pre>
<p>
We weren't able to complete your activation automatically, but don't worry, we'll finish
it manually if possible, and email you when that is done or if we need more information.
</p>
<hr>
</body>
</html>
"""


class HandlerBase(Resource):
    def __init__(self, out=None, *a, **kw):
        Resource.__init__(self, *a, **kw)
        self._log = logging.getLogger(self.__class__.__name__)
        self._log.debug('Initialized.')
        if out is None:
            out = sys.stdout
        self.out = out

    def render_POST(self, request):
        self.log_request(self, request)
        return self.render(self, request)

    def render_GET(self, request):
        self.log_request(self, request)
        return self.render(self, request)

    def log_request(self, request):
        details = dict(
            [ (k, getattr(request, k))
              for k in ['method',
                        'uri',
                        'path',
                        'args',
                        'received_headers']
              ])
        details['client-ip'] = request.getClientIP()
        self._log.debug('Request details from %r:\n%s', request, pprint.pformat(details))

    def get_arg(self, request, argname):
        try:
            [arg] = request.args[argname]
        except (KeyError, ValueError):
            arg = ""
        # We don't expect characters that need to be quoted, but quote the arguments
        # anyway, to avoid XSS when we interpolate them into HTML. URL-encoding is
        # safe for HTML tag attributes enclosed in "".
        return quote(arg, safe='@=:/+ ')

    def append_record(self, filename, *args):
        f = open(filename, "a+")
        try:
            f.write(",".join((format_iso_time(time.time()),) + args) + "\n")
        finally:
            f.close()


class DevPayPurchaseHandler(HandlerBase):
    def render(self, request):
        print >>self.out, "Ooh, possible signup coming:", request.args
        activationkey = self.get_arg(request, 'ActivationKey')
        productcode = self.get_arg(request, 'ProductCode')

        self.append_record("devpay_completions.csv", activationkey, productcode)

        request.setResponseCode(200)
        if activationkey and productcode:
            return DEVPAY_RESPONSE_HAVE_CODES_HTML % {"activationkey": activationkey,
                                                      "productcode": productcode}
        else:
            return DEVPAY_RESPONSE_MISSING_CODE_HTML


class RequestOutputStream(object):
    def __init__(self, request, tee=None):
        self.request = request
        self.tee = tee

    def write(self, s):
        # reject non-shortest-form encodings, which might defeat the escaping
        s.decode('utf-8', 'strict')
        self.request.write(htmlEscape(s))
        if self.tee:
            self.tee.write(s)

    def writelines(self, seq):
        for s in seq:
            self.write(s)

    def flush(self):
        pass

    def isatty(self):
        return False

    def close(self):
        pass


class NullOutputStream(object):
    def write(self, s):
        pass

    def writelines(self, seq):
        pass

    def flush(self):
        pass

    def isatty(self):
        return False

    def close(self):
        pass


def no_newlines(s):
    return s.replace('\n', '|').replace('\r', '')


flappcommand = FlappCommand("../signup.furl")

def start():
    return flappcommand.start()

class ActivationRequestHandler(HandlerBase):
    def render(self, request):
        print >>self.out, "Yay! Someone signed up :-)  ", request.args
        # TODO: check whether this is a known product for which signups are currently
        # enabled, and handle the case where the customer was already signed up.

        name = self.get_arg(request, 'Name')
        email = self.get_arg(request, 'Email')
        activationkey = self.get_arg(request, 'ActivationKey')
        productcode = self.get_arg(request, 'ProductCode')
        publickey = self.get_arg(request, 'PublicKey')

        self.append_record("activation_requests.csv", activationkey, productcode, name, email, publickey)

        request.setResponseCode(200)
        request.write(ACTIVATIONREQ_RESPONSE_HTML)

        stdin = ("%s\n"*5) % (no_newlines(activationkey),
                              no_newlines(productcode),
                              no_newlines(name),
                              no_newlines(email),
                              no_newlines(publickey),
                             )
        stdout = RequestOutputStream(request, tee=self.out)
        stderr = NullOutputStream()
        def when_done():
            self.append_record("signups.csv", 'success', activationkey, productcode, name, email, publickey)
            request.write(SUCCEEDED_HTML)
            request.finish()
        def when_failed():
            self.append_record("signups.csv", 'failure', activationkey, productcode, name, email, publickey)
            request.write(FAILED_HTML)
            request.finish()
        try:
            flappcommand.run(stdin, stdout, stderr, when_done, when_failed)
        except Exception, e:
            import traceback
            traceback.print_exc(100, self.out)
            when_failed()

        # http://twistedmatrix.com/documents/10.1.0/web/howto/web-in-60/asynchronous.html
        return NOT_DONE_YET
