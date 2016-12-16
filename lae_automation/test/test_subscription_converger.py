"""
Tests for ``lae_automation.subscription_converger``.
"""

from lae_util.testtools import TestCase

from lae_automation.subscription_converger import converge_service

class ConvergeServiceTests(TestCase):
    """
    Tests for ``converge_service``.
    """
    def test_empty_no_changes(self):
        """
        If there are no subscriptions in either place, nothing happens.
        """
        self.assertEqual(
            converge_service([], EMPTY_SERVICE),
            EMPTY_SERVICE,
        )

    def test_desired_subscription_missing(self):
        """
        If there is a subscription present in the desired list
        """
