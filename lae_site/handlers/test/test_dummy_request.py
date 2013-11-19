import sys, stripe
from mock import Mock, patch
from lae_site.handlers.subscription_complete import SubscriptionReportHandler


from twisted.web.test.test_web import DummyRequest
from twisted.python import log, filepath
from twisted.trial import unittest

def _render(resource, request):
    result = resource.render(request)
    if isinstance(result, str):
        request.write(result)
        request.finish()
        return succeed(None)
    elif result is server.NOT_DONE_YET:
        if request.finished:
            return succeed(None)
        else:
            return request.notifyFinish()
    else:
        raise ValueError("Unexpected return value: %r" % (result,))


class Tester(unittest.TestCase):

    #MockCreate = Mock(stripe.Customer.create)
    MockCustomer = Mock(spec=stripe.Customer)
    #MockCreate.return_value = MockCustomer
    configdict = {'email':"test@testemail", 
                  'default_card':"foro", 
                  'subscription':{'plan':{'name':"S4"}}, 
                  'id':"Waggle"}
    MockCustomer.configure_mock(**configdict)
    print dir(MockCustomer)
    print type(MockCustomer.id)
    print "MockCustomer.subscription.plan.name: %s" % MockCustomer.subscription.plan.name
    @patch('lae_site.handlers.subscription_complete.stripe.Customer', MockCustomer)
    def test_rendering(self):
        subscriptionreporthandler = SubscriptionReportHandler(filepath.FilePath('../../../_trial_temp'))
        request = DummyRequest([''])
        for k, v in (('stripeToken', ['flooble']), ('email', ['test@testmail']), ('nickname', ['poobles'])):
            request.addArg(k, v)
        d = _render(subscriptionreporthandler, request)
        def rendered(ignored):
            log.msg(request.responseCode)
            self.assertEquals(request.responseCode, 200)
            self.assertEquals("".join(request.written), "...")
            d.addCallback(rendered)
        return d
