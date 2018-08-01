
from zope.interface.verify import verifyObject

from testtools.matchers import IsInstance, Equals

from twisted.internet.interfaces import IProtocolFactory
from twisted.web.resource import IResource, Resource
from twisted.web.static import Data
from twisted.web.server import Site
from twisted.web.client import readBody
from twisted.web.http_headers import Headers

from twisted.python.filepath import FilePath
from twisted.test.proto_helpers import MemoryReactor

from treq.testing import RequestTraversalAgent

from lae_site.handlers import (
    make_resource,
    make_site,
    _ResourceWithHeaders,
)
from lae_site.main import (
    SiteOptions,
    site_for_options,
    ChargeBeeCreateSubscription,
    access_control_allow_origins,
)

from lae_util.memoryagent import dummyRequest, asResponse, render
from lae_util.testtools import TestCase


class MakeResourceTests(TestCase):
    """
    Tests for ``make_resource``.
    """
    def test_interface(self):
        """
        ``make_resource`` returns an ``IResource`` provider.
        """
        cross_domain = u"http://localhost:5000/"
        resource = make_resource(
            u"stripe-publishable-api-key",
            lambda: None,
            object(),
            object(),
            cross_domain,
        )
        verifyObject(IResource, resource)


class _ResourceWithHeadersTests(TestCase):
    """
    Tests for ``_ResourceWithHeaders``.
    """
    def test_extra_headers(self):
        """
        The extra headers passed to ``_ResourceWithHeaders`` are rendered into the
        response by ``_ResourceWithHeaders.render``.
        """
        resource = Data(b"foo", b"text/plain")
        wrapper = _ResourceWithHeaders(
            Headers({b"bar": [b"baz"]}),
            resource,
        )
        request = dummyRequest(b"GET", b"/", Headers())
        self.successResultOf(render(wrapper, request))
        response = asResponse(request)
        self.expectThat(
            response.headers,
            Equals(Headers({
                b"content-type": [b"text/plain"],
                b"content-length": [b"3"],
                b"bar": [b"baz"],
            }))
        )
        body = self.successResultOf(readBody(response))
        self.expectThat(body, Equals(b"foo"))


class MakeSiteTests(TestCase):
    """
    Tests for ``make_site``.
    """
    def test_interface(self):
        """
        ``make_site`` returns an ``IProtocolFactory`` provider.
        """
        logs = FilePath(self.mktemp().decode("ascii"))
        site = make_site(Resource(), logs)
        verifyObject(IProtocolFactory, site)



class SiteForOptionsTests(TestCase):
    """
    Tests for ``site_for_options``.
    """
    origin = b"http://127.0.0.1:5000"

    def setUp(self):
        TestCase.setUp(self)
        self.reactor = MemoryReactor()

    def make_site(self):
        p = FilePath(self.mktemp().decode("ascii"))
        p.makedirs()
        chargebee_secret = p.child(b"chargebee-key.secret")
        chargebee_secret.setContent(b"foo")
        stripe_publishable = p.child(b"stripe-key.publishable")
        stripe_publishable.setContent(b"bar")
        stripe_secret = p.child(b"stripe-key.secret")
        stripe_secret.setContent(b"baz")

        options = SiteOptions(self.reactor)
        options.parseOptions([
            b"--chargebee-domain", b"localhost",
            b"--chargebee-site-name", b"some-site",
            b"--chargebee-secret-api-key-path", chargebee_secret.path,
            b"--chargebee-plan-id", b"foo-bar",
            b"--chargebee-gateway-account-id", b"gw_baz",
            b"--stripe-publishable-api-key-path", stripe_publishable.path,
            b"--site-logs-path", b"httpd.log",
            b"--secure-port", b"tcp:0",
            b"--subscription-manager", b"http://127.0.0.1:8888/",
            b"--cross-domain", self.origin,
            b"--wormhole-result-path", self.mktemp(),
            b"--signup-failure-address", u"admin@example.invalid",
        ])
        return site_for_options(self.reactor, options)


    def test_stuff(self):
        site = self.make_site()
        # XXX Very weak assertion...
        self.assertThat(site, IsInstance(Site))


    def _cors_test(self, method, uri):
        site = self.make_site()
        agent = RequestTraversalAgent(site.resource)

        d = agent.request(method, uri)
        response = self.successResultOf(d)
        self.expectThat(
            response.headers.getRawHeaders("Access-Control-Allow-Origin"),
            Equals([self.origin]),
        )


    def test_cors_configuration(self):
        self._cors_test(
            b"GET",
            b"http://127.0.0.1/configuration",
        )


class ChargeBeeCreateSubscriptionTests(TestCase):
    origin = b"http://127.0.0.1:5000"

    def test_cors_chargebee(self):
        chargebee_create_subscription = Resource()
        chargebee_estimates = Resource()
        chargebee_estimates.putChild(
            "create_subscription",
            chargebee_create_subscription,
        )
        chargebee_v2 = Resource()
        chargebee_v2.putChild(
            "estimates",
            chargebee_estimates,
        )

        chargebee_api = Resource()
        chargebee_api.putChild(
            "v2",
            chargebee_v2,
        )
        chargebee_root = Resource()
        chargebee_root.putChild(
            "api",
            chargebee_v2,
        )

        chargebee = ChargeBeeCreateSubscription(
            RequestTraversalAgent(chargebee_root),
            "example-test",
            "foo",
            "chargebee.com",
        )
        root = Resource()
        root.putChild("create_subscription", chargebee)

        agent = RequestTraversalAgent(
            access_control_allow_origins([self.origin], root),
        )
        d = agent.request(
            b"POST",
            b"http://127.0.0.0.1/create_subscription")
        response = self.successResultOf(d)
        self.expectThat(
            response.headers.getRawHeaders("Access-Control-Allow-Origin"),
            Equals([self.origin]),
        )
