
from cStringIO import StringIO

from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.web.http import OK
from twisted.web.server import NOT_DONE_YET
from twisted.python.filepath import FilePath

from lae_site.handlers import devpay_complete
from lae_site.config import Config


SITE_CONFIG_JSON = """{
  "products": [
    { "short_name":    "product",
      "full_name":     "Yummy cloud hotness for everyone",
      "listed":        "true",
      "signup_url":    "https://example.com/abc?offeringCode=12345678",
      "product_code":  "PRODUCTCODE"
    }
  ]
}"""


def remove_if_possible(fp):
    try:
        fp.remove()
    except EnvironmentError:
        if fp.exists():
            raise


class MockFlappCommand(object):
    def __init__(self, furlfile):
        self.should_succeed = True
    def start(self):
        return defer.succeed(None)
    def run(self, content, stdout, stderr, when_done, when_failed):
        print >>stdout, "Starting..."
        if self.should_succeed:
            when_done()
        else:
            print >>stdout, "Command failed with exit code 1."
            when_failed()


class MockRequest(object):
    def __init__(self, method, d, args):
        self.responsecode = None
        self.out = StringIO()
        self.method = method
        self.args = args
        self.d = d
    def setResponseCode(self, code):
        self.responsecode = code
    def write(self, s):
        self.out.write(s)
    def finish(self):
        self.d.callback(self.out.getvalue())


class Handlers(TestCase):
    def setUp(self):
        self.patch(devpay_complete, 'FlappCommand', MockFlappCommand)
        self.basefp = FilePath('.')
        remove_if_possible(self.basefp.child("activation_requests.csv"))
        remove_if_possible(self.basefp.child("signups.csv"))

    def _test_devpaypurchasehandler(self, method):
        out = StringIO()
        config = Config(StringIO(SITE_CONFIG_JSON))

        d = devpay_complete.start(self.basefp)
        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.DevPayPurchaseHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY"], ProductCode=["PRODUCTCODE"]))

        def _finished( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn('name="ActivationKey" value="ACTIVATIONKEY"', output)
            self.failUnlessIn('name="ProductCode" value="PRODUCTCODE"', output)
            self.failUnlessIn("ACTIVATIONKEY,PRODUCTCODE\n",
                              FilePath("devpay_completions.csv").getContent())
        d.addCallback(_finished)
        return d

    def test_devpaypurchasehandler_GET(self):
        return self._test_devpaypurchasehandler('GET')

    def test_devpaypurchasehandler_POST(self):
        return self._test_devpaypurchasehandler('POST')

    def _test_activationrequesthandler(self, method):
        out = StringIO()
        config = Config(StringIO(SITE_CONFIG_JSON))

        d = devpay_complete.start(self.basefp)
        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.ActivationRequestHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY"], ProductCode=["PRODUCTCODE"],
                                         Name=["Joe & Mildred"], Email=["joe+mildred@example.org"], PublicKey=["===BEGIN BLAH==="]))

        def _finished( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn("<title>Activation requested</title>", output)
            self.failUnlessIn("Your sign-up is complete.", output)
            self.failUnlessIn(",ACTIVATIONKEY,PRODUCTCODE,Joe %26 Mildred,joe+mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("activation_requests.csv").getContent())
            self.failUnlessIn(",success,ACTIVATIONKEY,PRODUCTCODE,Joe %26 Mildred,joe+mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("signups.csv").getContent())
        d.addCallback(_finished)

        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.ActivationRequestHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY"], ProductCode=["PRODUCTCODE"],
                                         Name=["Joe & Mildred"], Email=["joe+mildred@example.org"], PublicKey=["===BEGIN BLAH==="]))

        def _finished_again( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn("<title>Activation already succeeded</title>", output)

            devpay_complete.flappcommand.should_succeed = False
        d.addCallback(_finished_again)

        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.ActivationRequestHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY2"], ProductCode=["PRODUCTCODE"],
                                         Name=[""], Email=["joe+mildred@example.org"], PublicKey=["===BEGIN BLAH==="]))
        def _no_name( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn("<title>Missing name</title>", output)
            self.failUnlessIn("The 'Name' field was not filled in", output)
            self.failUnlessIn(",ACTIVATIONKEY2,PRODUCTCODE,,joe+mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("activation_requests.csv").getContent())
            self.failIfIn("ACTIVATIONKEY2", FilePath("signups.csv").getContent())
        d.addCallback(_no_name)

        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.ActivationRequestHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY3"], ProductCode=["PRODUCTCODE"],
                                         Name=["Joe & Mildred"], Email=["joe&mildred@example.org"], PublicKey=["===BEGIN BLAH==="]))
        def _bad_email( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn("<title>Missing or invalid email address</title>", output)
            self.failUnlessIn("The 'Email address' field was not filled in with a valid-looking address", output)
            self.failUnlessIn(",ACTIVATIONKEY3,PRODUCTCODE,Joe %26 Mildred,joe%26mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("activation_requests.csv").getContent())
            self.failIfIn("ACTIVATIONKEY3", FilePath("signups.csv").getContent())
        d.addCallback(_bad_email)

        d.addCallback(lambda ign:
                      self._mock_request(devpay_complete.ActivationRequestHandler(self.basefp, config.products, out=out),
                                         method, ActivationKey=["ACTIVATIONKEY4"], ProductCode=["PRODUCTCODE"],
                                         Name=["Joe & Mildred"], Email=["joe+mildred@example.org"], PublicKey=["===BEGIN BLAH==="]))
        def _finished_error( (req, output) ):
            self.failUnlessEqual(req.responsecode, OK)
            self.failUnlessIn("<title>Activation requested</title>", output)
            self.failUnlessIn("We weren't able to complete your sign-up automatically", output)
            self.failUnlessIn(",ACTIVATIONKEY4,PRODUCTCODE,Joe %26 Mildred,joe+mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("activation_requests.csv").getContent())
            self.failUnlessIn(",failure,ACTIVATIONKEY4,PRODUCTCODE,Joe %26 Mildred,joe+mildred@example.org,===BEGIN BLAH===\n",
                              FilePath("signups.csv").getContent())
        d.addCallback(_finished_error)
        return d

    def test_activationrequesthandler_GET(self):
        return self._test_activationrequesthandler('GET')

    def test_activationrequesthandler_POST(self):
        return self._test_activationrequesthandler('POST')

    def _mock_request(self, handler, method, **args):
        d = defer.Deferred()
        req = MockRequest(method, d, args)
        resp = handler.render(req)
        if resp == NOT_DONE_YET:
            d.addCallback(lambda output: (req, output))
        else:
            d.callback((req, resp))
        return d
