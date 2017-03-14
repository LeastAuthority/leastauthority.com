
from testtools.matchers import ContainsAll

from twisted.plugin import getPlugins
from twisted.application.service import IServiceMaker

from lae_util.testtools import TestCase


class PluginTests(TestCase):
    def test_discovery(self):
        """
        Our Twisted plugins can be discovered by ``getPlugins``.
        """
        plugin_names = {
            plugin.tapname
            for plugin
            in getPlugins(IServiceMaker)
        }
        self.expectThat(
            plugin_names,
            ContainsAll([
                u"s4-subscription-manager",
                u"s4-subscription-converger",
                u"s4-grid-router",
            ]),
        )
