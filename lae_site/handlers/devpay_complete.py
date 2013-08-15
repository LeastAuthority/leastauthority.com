
import logging, pprint, sys, traceback, re
from urllib import quote
from cgi import escape as htmlEscape

from twisted.internet import defer
from twisted.web.resource import Resource
from twisted.web.server import NOT_DONE_YET

from lae_util.flapp import FlappCommand
from lae_util.servers import append_record
from lae_util.send_email import send_plain_email, FROM_EMAIL, FROM_ADDRESS

from lae_site.handlers.web import env


EMAILS_FILE              = 'emails.csv'
DEVPAY_COMPLETIONS_FILE  = 'devpay_completions.csv'
ACTIVATION_REQUESTS_FILE = 'activation_requests.csv'
SIGNUPS_FILE             = 'signups.csv'
SIGNUP_FURL_FILE         = 'signup.furl'

VALID_EMAIL_RE = re.compile("[^@]+@[^@]+")


def html(title, body):
    return """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link href="/static/img/icon.png" rel="shortcut icon">
  <title>%s</title>
</head>
<body style="background: #FFFFFF">%s""" % (title, body)
    return ""


ACTIVATION_FORM_HTML = """<p>
The activation of your account is not quite finished; to complete it,
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
    <td><input type="text" id="ProductName" name="ProductName" value="%(productfullname)s" disabled style="width:20em">
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
If this form does not work for any reason, you can also send the same information
by email to <a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
<p>
Happy Simple Secure Storing!
</p>
<p>
The Least Authority Enterprises team (Zooko, Daira, Za, Leif, and Nathan)
</p>
<hr>
</body>
</html>
"""

# TODO: allow customizing the text per-product.
DEVPAY_RESPONSE_HAVE_CODES_HTML = html("Activation for Simple Secure Storage Service", """
<p>
Thank you for starting to activate your Simple Secure Storage Service!
</p>
""" + ACTIVATION_FORM_HTML)

# Amazon sometimes doesn't tell us the product code and/or activation key.
# In that case, the easiest way to get those codes is for the user to click on
# the 'Go to Application' link, so tell them to do that, rather than requiring
# them to paste in the code(s).
DEVPAY_RESPONSE_MISSING_CODE_HTML = html("Activation for Simple Secure Storage Service", """
<p>
Thank you for starting to activate your Simple Secure Storage Service!
</p>
<p>
Please follow the link above that says "click here to get activation
keys". It will take you to the next page. On that page you do not need to do
the <b>Generate Key</b> step &mdash; just go ahead and click the <b>Go to
Application</b> link for this subscription.
</p>
<p>
If you have any problem completing the sign-up, please contact
<a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
<p>
Happy Simple Secure Storing!
</p>
<p>
The Least Authority Enterprises team (Zooko, Daira, Za, Leif, and Nathan)
</p>
<hr>
</body>
</html>
""")

ACTIVATIONREQ_RESPONSE_NORMAL_HTML = html("Activation requested", """
<p>
Your activation request has been received.
</p>
<p>
When your account is activated, we will send you a confirmation email with
instructions on how to start using Simple Secure Storage Service, or alternatively
an email requesting any information that was missing. If you don't receive this
within an hour (also check your spam folder in case it is there), please contact
<a href="mailto:support@LeastAuthority.com">&lt;support@LeastAuthority.com&gt</a>.
</p>
<p>
Thanks again for signing up.
</p>
<p>
The Least Authority Enterprises team (Zooko, Daira, Za, Leif, and Nathan)
</p>
<hr>
<p>
The progress of your server setup is shown below. You do not need to read and
understand it -- it is shown solely to let you know that progress is being
made. Once the server setup is complete, an email will be sent to you with
all the information you need to know.
</p>
<p>
Please don't reload this page, since the activation key is
only valid once.
</p>
<pre>
""")

ACTIVATIONREQ_RESPONSE_MISSING_KEY_HTML = html("Missing activation key or product code", """
<p>
The activation key or product code was missing from the request. This can happen in some cases
if you reload the page after an activation request. If you are having any difficulty signing up,
please contact <a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
</p>
<pre>
""")

ACTIVATIONREQ_RESPONSE_MISSING_NAME_HTML = html("Missing name", """
<p style="color: red;">
The 'Name' field was not filled in. All fields except for the PGP key information
are required. If you are having any difficulty signing up, please contact
<a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
</p>
<hr>
""" + ACTIVATION_FORM_HTML)

ACTIVATIONREQ_RESPONSE_MISSING_OR_INVALID_EMAIL_HTML = html("Missing or invalid email address", """
<p style="color: red;">
The 'Email address' field was not filled in with a valid-looking address. All fields except for
the PGP key information are required. If you are having any difficulty signing up, please contact
<a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
</p>
<hr>
""" + ACTIVATION_FORM_HTML)

ACTIVATIONREQ_RESPONSE_ALREADY_SUCCEEDED_HTML = html("Activation already succeeded", """
<p>
This activation key has already been used in a successful sign-up. If you have not received
the confirmation e-mail, please contact
<a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
</p>
<hr>
</body>
</html>
""")

ACTIVATIONREQ_RESPONSE_ALREADY_USED_HTML = html("Activation key already used", """
<p>
This activation key has already been used. If you have not yet received any e-mail
about your sign-up, please contact
<a href="mailto:support@leastauthority.com">&lt;support@leastauthority.com&gt</a>.
</p>
<hr>
</body>
</html>
""")

SUCCEEDED_HTML = """</pre>
<p>
Your Simple Secure Storage Service has been activated!
</p>
<p>
Please go to
<a href="https://leastauthority.com/howtoconfigure" target="_blank">https://leastauthority.com/howtoconfigure</a>
for instructions on how to set up and configure your Tahoe-LAFS gateway
(using the <tt>introducer.furl</tt> printed above) and start using the service.
</p>
<p>
You will also shortly get a confirmation email at the address you provided.
</p>
<hr>
</body>
</html>
"""

FAILED_HTML = """</pre>
<p>
We weren't able to complete your account activation automatically, but don't worry,
we'll finish it manually if possible, and email you when that is done or if we need
more information.
</p>
<hr>
</body>
</html>
"""

AUTOREPLY_EMAIL_SUBJECT = "Thank you for your interest in %(full_product_name)s"

AUTOREPLY_NOT_READY_EMAIL_BODY = """Hello,

We've received your request to sign up for %(full_product_name)s.
When we're ready to sign you up, we'll send another email.

If you have any questions in the meantime, please email them to
<support@LeastAuthority.com>.

Thanks for your patience.

--\x20
The Least Authority Enterprises team
"""

AUTOREPLY_READY_EMAIL_BODY = """Hello,

We've received your request to sign up for %(full_product_name)s.

Please go to %(signup_url)s to confirm your sign up and
payment details with Amazon Payments, and initiate the activation process.

If you have any questions, please email them to <support@LeastAuthority.com>.

Thanks!

--\x20
The Least Authority Enterprises team
"""


def get_full_name(productcode, products):
    matches = [p['full_name'] for p in products if p['product_code'] == productcode]
    if len(matches) != 1:
        return "Unknown"
    return matches[0]


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

        # Quote the arguments to avoid injection attacks when we interpolate them into HTML
        # attributes (enclosed in ""), CSV values, or the stdin of the flapp command.
        # We could use htmlEscape, but that does not escape ',' or newlines so we would need
        # additional quoting for CSV and flapp.
        # URL-encoding with the set of safe-characters below will work for all these cases.
        # Note that the safe set must include characters that are valid in email addresses.
        return quote(arg, safe=' !#$()*+-./:=?@^_`{|}~')


class CollectEmailHandler(HandlerBase):
    def __init__(self, basefp, products, out=None):
        HandlerBase.__init__(self, out=out)
        self.basefp = basefp
        self.products = products

    def render(self, request):
        print >>self.out, "Yay, another potential customer:", request.args
        email = self.get_arg(request, 'Email')
        productname = self.get_arg(request, 'ProductName')
        productfullname = self.get_arg(request, 'ProductFullName')

        append_record(self.basefp.child(EMAILS_FILE), email, productname)

        request.setResponseCode(200)

        if email and VALID_EMAIL_RE.match(email):
            tmpl = env.get_template('valid_email.html')
            (start, _, rest) = tmpl.render(productname=productname, productfullname=productfullname).encode('utf-8').partition("MAGIC")
            request.write(start)

            d = defer.succeed(None)
            d.addCallback(lambda ign: send_autoreply(email, productfullname, 'https://leastauthority.com/signup/s4'))
            def _sent(ign):
                request.write("We've sent you an email to check that your address is working, that email contains the "
                              "activation URL you'll use to continue the activation process.")
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


def send_autoreply(customer_email, full_product_name, signup_url):
    # TODO: the name is URL-escaped UTF-8. It should be OK to unescape it since the email is plain text,
    # but I'm being cautious for now since I haven't reviewed email.mime.text.MIMEText to make sure that's safe.
    if signup_url is None:
        content = AUTOREPLY_NOT_READY_EMAIL_BODY % {
                   "full_product_name": full_product_name,
                  }
    else:
        content = AUTOREPLY_READY_EMAIL_BODY % {
                   "full_product_name": full_product_name,
                   "signup_url": signup_url,
                  }

    headers = {
               "From": FROM_ADDRESS,
               "Subject": AUTOREPLY_EMAIL_SUBJECT % {"full_product_name": full_product_name},
              }

    d = send_plain_email(FROM_EMAIL, customer_email, content, headers)
    return d


class DevPayPurchaseHandler(HandlerBase):
    def __init__(self, basefp, products, out=None):
        HandlerBase.__init__(self, out=out)
        self.basefp = basefp
        self.products = products

    def render(self, request):
        print >>self.out, "Ooh, possible signup coming:", request.args
        activationkey = self.get_arg(request, 'ActivationKey')
        productcode = self.get_arg(request, 'ProductCode')

        append_record(self.basefp.child(DEVPAY_COMPLETIONS_FILE), activationkey, productcode)

        request.setResponseCode(200)
        if activationkey and productcode:
            return DEVPAY_RESPONSE_HAVE_CODES_HTML % {"activationkey": activationkey,
                                                      "productcode": productcode,
                                                      "productfullname": get_full_name(productcode, self.products)}
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


# Rely on start() to initialize these, in order to make it easier for tests to patch
# and/or reinitialize.
flappcommand = None
all_activationkeys = None
successful_activationkeys = None

def start(basefp):
    global flappcommand, all_activationkeys, successful_activationkeys

    signup_furl_fp = basefp.child('secret_config').child(SIGNUP_FURL_FILE)
    activation_requests_fp = basefp.child(ACTIVATION_REQUESTS_FILE)
    signups_fp = basefp.child(SIGNUPS_FILE)

    flappcommand = FlappCommand(signup_furl_fp.path)
    all_activationkeys = set([])
    successful_activationkeys = set([])

    # read the sets of keys
    try:
        f = activation_requests_fp.open("r")
    except IOError:
        if activation_requests_fp.exists():
            raise
    else:
        try:
            for line in f:
                fields = line.split(',')
                if len(fields) >= 2:
                    [timestamp, key] = fields[:2]
                    all_activationkeys.add(key)
        finally:
            f.close()

    try:
        f = signups_fp.open("r")
    except IOError:
        if signups_fp.exists():
            raise
    else:
        try:
            for line in f:
                fields = line.split(',')
                if len(fields) >= 3:
                    [timestamp, outcome, key] = fields[:3]
                    if outcome == "success":
                        successful_activationkeys.add(key)
        finally:
            f.close()

    return flappcommand.start()


class ActivationRequestHandler(HandlerBase):
    def __init__(self, basefp, products, out=None):
        HandlerBase.__init__(self, out=out)
        self.basefp = basefp
        self.products = products

    def render(self, request):
        print >>self.out, "Got activation request:", request.args

        name = self.get_arg(request, 'Name')
        email = self.get_arg(request, 'Email')
        activationkey = self.get_arg(request, 'ActivationKey')
        productcode = self.get_arg(request, 'ProductCode')
        publickey = self.get_arg(request, 'PublicKey')

        activation_requests_fp = self.basefp.child(ACTIVATION_REQUESTS_FILE)
        signups_fp = self.basefp.child(SIGNUPS_FILE)

        append_record(activation_requests_fp, activationkey, productcode, name, email, publickey)

        request.setResponseCode(200)

        if not (activationkey and productcode):
            return ACTIVATIONREQ_RESPONSE_MISSING_KEY_HTML
        elif activationkey in successful_activationkeys:
            return ACTIVATIONREQ_RESPONSE_ALREADY_SUCCEEDED_HTML
        elif activationkey in all_activationkeys:
            return ACTIVATIONREQ_RESPONSE_ALREADY_USED_HTML
        elif not name:
            return ACTIVATIONREQ_RESPONSE_MISSING_NAME_HTML % {"activationkey": activationkey,
                                                               "productcode": productcode,
                                                               "productfullname": get_full_name(productcode, self.products)}
        elif "%" in email or not "@" in email:
            return ACTIVATIONREQ_RESPONSE_MISSING_OR_INVALID_EMAIL_HTML % {"activationkey": activationkey,
                                                                           "productcode": productcode,
                                                                           "productfullname": get_full_name(productcode, self.products)}

        print >>self.out, "Yay! Someone signed up :-)"

        request.write(ACTIVATIONREQ_RESPONSE_NORMAL_HTML)

        # None of these fields can contain newlines because they would be quoted by get_arg.
        stdin = ("%s\n"*5) % (activationkey,
                              productcode,
                              name,
                              email,
                              publickey,
                             )
        stdout = RequestOutputStream(request, tee=self.out)
        stderr = self.out
        def when_done():
            try:
                successful_activationkeys.add(activationkey)
                all_activationkeys.add(activationkey)
                append_record(signups_fp, 'success', activationkey, productcode, name, email, publickey)
            except Exception:
                # The request really did succeed, we just failed to record that it did. Log the error locally.
                traceback.print_exc(100, stderr)
                request.write(SUCCEEDED_HTML)
            else:
                request.write(SUCCEEDED_HTML)
            finally:
                request.finish()
        def when_failed():
            try:
                all_activationkeys.add(activationkey)
                append_record(signups_fp, 'failure', activationkey, productcode, name, email, publickey)
            except Exception:
                traceback.print_exc(100, stderr)
                request.write(FAILED_HTML)
            else:
                request.write(FAILED_HTML)
            finally:
                request.finish()
        try:
            flappcommand.run(stdin, stdout, stderr, when_done, when_failed)
        except Exception:
            traceback.print_exc(100, stdout)
            when_failed()

        # http://twistedmatrix.com/documents/10.1.0/web/howto/web-in-60/asynchronous.html
        return NOT_DONE_YET
