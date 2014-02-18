
from twisted.trial.unittest import TestCase

from lae_site.handlers import submit_subscription

class SubmitSubscriptionHandler(TestCase):
    def setUp(self):
        fc, stripe, ws_env = submit_subscription.flappcommand, submit_subscription.stripe, submit_subscription.env

        submit_subscription.flappcommand = "MOCKFLAPPCOMMAND"
        submit_subscription.stripe = "MOCKSTRIPE"
        submit_subscription.env = "MOCKENVIRONMENT"
    
