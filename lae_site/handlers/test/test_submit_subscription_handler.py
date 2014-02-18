
from twisted.trial.unittest import TestCase

from lae_site.handlers import submit_subscription


class MockFlappCommand(object):
    def __init__(self, furlfile):
        self.should_succeed = True
    def start(self):
        return defer.succeed(None)
    def run(self, content, stdout):
        print >>stdout, "Starting..."
        return defer.succeed(None)

class MockStripe(object):
    def __init__(self):
        pass

class MockEnv(object):
    def __init__(self):
        pass

class TestSubmitSubscriptionHandler(TestCase):
    def setUp(self):
        stripe, ws_env = submit_subscription.stripe, submit_subscription.env
        self.patch(submit_subscription, 'FlappCommand', MockFlappCommand)
        self.patch(submit_subscription, 'stripe', MockStripe)
        self.patch(submit_subscription, 'env', MockEnv)
        
    def tearDown(self):
        pass
