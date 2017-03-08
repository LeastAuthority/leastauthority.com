
from zope.interface.verify import verifyObject

from twisted.internet.interfaces import IProtocolFactory
from twisted.web.resource import IResource, Resource
from twisted.python.filepath import FilePath

from lae_site.handlers import make_resource, make_site

from lae_util.testtools import TestCase


SITE_CONFIG_JSON = """{}"""
STRIPE_PUBLISHABLE_API_KEY = b"abcdef"
STRIPE_API_KEY = b"abcdef"

class MakeResourceTests(TestCase):
    """
    Tests for ``make_resource``.
    """
    def test_interface(self):
        """
        ``make_resource`` returns an ``IResource`` provider.
        """
        p = FilePath(self.mktemp().decode("ascii"))
        p.makedirs()
        resource = make_resource(
            p.child(u"email"),
            u"stripe-secret-api-key",
            u"stripe-publishable-api-key",
            p.child(u"confirmed"),
            p.child(u"subscriptions"),
        )
        verifyObject(IResource, resource)


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
