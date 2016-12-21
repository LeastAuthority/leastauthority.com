"""
Tests for ``lae_automation.subscription_converger``.
"""

from hypothesis import given
from hypothesis.strategies import lists

from testtools.matchers import Equals

from lae_util.testtools import TestCase

from lae_automation.subscription_converger import (
    converge_service,
)
from lae_automation.containers import (
    EMPTY_SERVICE,
    service_ports,
    introducer_port_name,
    storage_port_name,
    add_subscription_to_service,
)

from .strategies import subscription_details, deployment_configuration

class ConvergeServiceTests(TestCase):
    """
    Tests for ``converge_service`` and helpers.
    """
    @given(subscription_details())
    def test_service_ports(self, details):
        """
        ``service_ports`` returns a two-element list containing mappings
        which expose the subscription's introducer and storage ports.
        """
        self.assertThat(
            service_ports(details),
            Equals([
                {
                    "name": introducer_port_name(details.subscription_id),
                    "port": details.introducer_port_number,
                    "targetPort": introducer_port_name(details.subscription_id),
                    "protocol": "TCP",
                },
                {
                    "name": storage_port_name(details.subscription_id),
                    "port": details.storage_port_number,
                    "targetPort": storage_port_name(details.subscription_id),
                    "protocol": "TCP",
                },
            ]),
        )

    def test_empty_no_changes(self):
        """
        If there are no subscriptions in either place, nothing happens.
        """
        self.assertThat(
            converge_service(set(), EMPTY_SERVICE),
            Equals(EMPTY_SERVICE),
        )

    @given(details=subscription_details())
    def test_configured_subscription_missing(self, details):
        """
        If there is a subscription present in the desired list but not in
        the actual state, a configuration change to add that
        subscription is emitted.
        """
        one_subscription = add_subscription_to_service(EMPTY_SERVICE, details)
        self.assertThat(
            one_subscription,
            converge_service({details.subscription_id}, EMPTY_SERVICE),
        )

    @given(details=subscription_details())
    def test_unconfigured_subscription_present(self, details):
        """
        If there is a subscription present in the actual state that does
        not exist in the subscription list, a configuration change to
        remove that subscription is emitted.
        """
        one_subscription = add_subscription_to_service(EMPTY_SERVICE, details)
        self.assertThat(
            EMPTY_SERVICE,
            converge_service(set(), one_subscription),
        )
