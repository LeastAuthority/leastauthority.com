
from cStringIO import StringIO

from twisted.internet import defer
from twisted.trial.unittest import TestCase
from twisted.web.server import NOT_DONE_YET
from twisted.python.filepath import FilePath

from lae_site.handlers import subscription_complete


class MockFlappCommand(object):
    def __init__(self, furlfile):
        self.should_succeed = True
    def start(self):
        return defer.succeed(None)
    def run(self, content, stdout, stderr, when_done, when_failed):
        for char in content:
            assert isinstance(char, bytes)
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


class MockCustomer(object):
    def create(self, **args):
        self.id = 'Mockid'
        self.default_card = 'MOCK_default_card'
        self.email = args['email']
        self.subscription = MockSubscription()
        return self

class MockSubscription(object):
    def __init__(self):
        self.id = 'RANDOMISHSTRING'
        self.plan = MockPlan()

class MockPlan(object):
    def __init__(self):
        self.name = 'MOCKS4'
        
class MockStripe(object):
    def __init__(self):
        self.Customer = MockCustomer()
        self.CardError = 'MockCardError'

def mock_create_secrets_file(basefp, timestamp, stripe_customer_id):
    logfilename = "%s-%s" % (timestamp, stripe_customer_id)

    secretsfile = basefp.child('secrets_'+logfilename)
    logfile = basefp.child('signup_logs_'+logfilename)
    return secretsfile, logfile

def mock_callLater(*args):
    pass

class Handlers(TestCase):

    def setUp(self):
        self.patch(subscription_complete, 'FlappCommand', MockFlappCommand)
        self.patch(subscription_complete, 'stripe', MockStripe())
        self.patch(subscription_complete, 'create_secrets_file', mock_create_secrets_file)
        self.basefp = FilePath('.')

    def _test_subscriptionreporthandler(self, method, **args):
        d = subscription_complete.start(self.basefp)
        d.addCallback(lambda ign: self._mock_request(
                subscription_complete.SubscriptionReportHandler(self.basefp), method, 
                **args))
        return d

    def _mock_request(self, handler, method, **args):
        d = defer.Deferred()
        req = MockRequest(method, d, args)
        resp = handler.render(req)
        if resp == NOT_DONE_YET:
            d.addCallback(lambda output: (req, output))
        else:
            d.callback((req, resp))
        return d
        
