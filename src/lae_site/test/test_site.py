
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

from lae_site.handlers import (
    make_resource,
    make_site,
    _ResourceWithHeaders,
)
from lae_site.main import SiteOptions, site_for_options

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
            u"stripe-plan-id",
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
    def test_stuff(self):
        p = FilePath(self.mktemp().decode("ascii"))
        p.makedirs()
        chargebee_secret = p.child(b"chargebee-key.secret")
        chargebee_secret.setContent(b"foo")
        stripe_publishable = p.child(b"stripe-key.publishable")
        stripe_publishable.setContent(b"bar")

        options = SiteOptions(MemoryReactor())
        options.parseOptions([
            b"--chargebee-site-name", b"some-site",
            b"--chargebee-secret-api-key-path", chargebee_secret.path,
            b"--stripe-publishable-api-key-path", stripe_publishable.path,
            b"--chargebee-plan-id", b"foo-bar",
            b"--site-logs-path", b"httpd.log",
            b"--secure-port", b"tcp:0",
            b"--subscription-manager", b"http://127.0.0.1:8888/",
            b"--cross-domain", b"http://127.0.0.1:5000/",
            b"--wormhole-result-path", self.mktemp(),
        ])
        site = site_for_options(MemoryReactor(), options)
        # XXX Very weak assertion...
        self.assertThat(site, IsInstance(Site))
