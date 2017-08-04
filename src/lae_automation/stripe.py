
from __future__ import unicode_literals

from json import dumps

import attr
from attr.validators import instance_of

from twisted.web.resource import Resource
from twisted.web.client import Agent

import treq
from treq.client import HTTPClient
from treq.auth import add_basic_auth
from treq.testing import StubTreq

from hyperlink import URL


def _stripe_resource(state):
    v1 = Resource()
    v1.putChild(b"subscriptions", _Subscriptions(state))

    root = Resource()
    root.putChild(b"v1", v1)
    return root



def memory_stripe_client(state):
    return _StripeClient(
        b"http://127.0.0.0/",
        StubTreq(_stripe_resource(state)),
    )


def network_stripe_client(reactor, api_key):
    return _StripeClient(
        b"https://api.stripe.com/",
        HTTPClient(
            add_basic_auth(Agent(reactor), api_key, b""),
        ),
    )


class StripeState(object):
    def __init__(self):
        self.subscriptions = {}



@attr.s(frozen=True)
class _StripeClient(object):
    root_url = attr.ib()
    treq = attr.ib()

    def list_subscriptions(self, starting_after=None, limit=None):
        url = URL.from_text(self.root_url).child(u"v1", u"subscriptions")
        if starting_after is not None:
            url = url.add(u"starting_after", starting_after)
        if limit is not None:
            url = url.add(u"limit", u"{}".format(limit))

        d = self.treq.get(url.to_uri().to_text())
        d.addCallback(treq.json_content)
        d.addCallback(
            lambda stripe_list: list(
                StripeSubscription(
                    id=subscr["id"],
                    status=subscr["status"],
                )
                for subscr
                in stripe_list["data"]
            )
        )
        return d



@attr.s(frozen=True)
class StripeSubscription(object):
    id = attr.ib(validator=instance_of(unicode))

    # trialing active past_due canceled unpaid
    status = attr.ib(validator=instance_of(unicode))



class _Subscriptions(Resource):
    def __init__(self, state):
        Resource.__init__(self)
        self._state = state


    def render_GET(self, request):
        # https://stripe.com/docs/api#list_subscriptions
        return dumps(
            _stripe_list(
                self._state.subscriptions.values(),
                "/v1/subscriptions",
            ),
        )



def _stripe_list(objs, url, has_more=False):
    return dict(
        object="list",
        url=url,
        has_more=has_more,
        data=list(
            attr.asdict(o)
            for o
            in objs
        ),
    )
