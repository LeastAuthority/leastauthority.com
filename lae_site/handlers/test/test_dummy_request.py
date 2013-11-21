import sys, stripe, simplejson
from mock import Mock, patch
from lae_site.handlers.subscription_complete import SubscriptionReportHandler


from twisted.web.server import NOT_DONE_YET
from twisted.web.test.test_web import DummyRequest
from twisted.python import log, filepath
from twisted.trial import unittest

subscriptionexample_json = ur"""{ "id": "sub_2yfHiDj4ChHAU7", "plan": { "interval": "month", "name": "tiny pumpkin -d metadata[size]=tiny\n", "amount": 100, "currency": "usd", "id": "JAVA-PLAN-a7761760-534c-41b2-85c0-2006ac8f6d0c", "object": "plan", "livemode": false, "interval_count": 1, "trial_period_days": null, "metadata": { "size": "tiny" } }, "object": "subscription", "start": 1384980687, "status": "active", "customer": "cus_2ycwnWS4KgamXZ", "cancel_at_period_end": false, "current_period_start": 1384980687, "current_period_end": 1387572687, "ended_at": null, "trial_start": null, "trial_end": null, "canceled_at": null, "quantity": 1, "application_fee_percent": null }"""

def _render(resource, request):
    result = resource.render(request)
    if isinstance(result, str):
        request.write(result)
        request.finish()
        return succeed(None)
    elif result is NOT_DONE_YET:
        if request.finished:
            return succeed(None)
        else:
            return request.notifyFinish()
    else:
        raise ValueError("Unexpected return value: %r" % (result,))


class Tester(unittest.TestCase):
    subscription_dict = simplejson.loads(subscriptionexample_json)
    #MockCreate = Mock(stripe.Customer.create)
    MockSubscription = Mock(subscription_dict)
    MockCustomer = Mock(spec=stripe.Customer)
    #MockCreate.return_value = MockCustomer
    configdict = {'email':"test@testemail", 
                  'default_card':"foro", 
                  'subscription':Mock(),
                  'id':"Waggle"}
    MockCustomer.configure_mock(**configdict)
    MockCustomer.subscription.plan.configure_mock(name='S4')
    MockCustomer.subscription.configure_mock(id='subid')
    MockCustomer.create.return_value = MockCustomer
    Mockflappcommand = Mock()
    Mockflappcommand.run.return_value = True

    @patch('lae_site.handlers.subscription_complete.flappcommand', Mockflappcommand)
    @patch('lae_site.handlers.subscription_complete.all_subscribed', set([]))
    @patch('lae_site.handlers.subscription_complete.stripe.Customer', MockCustomer)
    def test_rendering(self):
        basefp = filepath.FilePath('.').child('_rendering')
        basefp.createDirectory()
        SECRETSDIRFP = basefp.child('secrets')
        SECRETSDIRFP.createDirectory()
        LOGGINGDIRFP = basefp.child('signup_logs')
        LOGGINGDIRFP.createDirectory()
        subscriptionreporthandler = SubscriptionReportHandler(basefp)
        request = DummyRequest([''])
        for k, v in (('stripeToken', ['flooble']), ('email', ['test@testmail']), ('nickname', ['poobles']), ('pgp_pubkey', [''])):
            request.addArg(k, v)
        d = _render(subscriptionreporthandler, request)
        def rendered(ignored):
            log.msg(request.responseCode)
            self.assertEquals(request.responseCode, 200)
            self.assertEquals("".join(request.written), "...")
            d.addCallback(rendered)
        return d
