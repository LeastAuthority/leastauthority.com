# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_automation.subscription_manager``.
"""

from tempfile import mkdtemp

import attr

from zope.interface.verify import verifyObject

from twisted.python.filepath import FilePath
from twisted.application.service import IService

from testtools.matchers import Equals, Is, Not

from hypothesis import given, assume

from lae_automation.subscription_manager import (
    Options, makeService, memory_client,
)

from lae_util.testtools import TestCase

from .strategies import subscription_id, subscription_details
from .matchers import AttrsEquals


def partial_subscription_details():
    return subscription_details().map(
        lambda d: attr.assoc(d, oldsecrets=None),
    )


class SubscriptionManagerTestMixin(object):
    """
    Tests for the subscription manager's HTTP interface.

    Subclasses will mix this in to define tests against the subscription
    manager client interface.  They must override ``get_client`` to create a
    client to subject to the tests.
    """
    def get_client(self):
        raise NotImplementedError()


    @given(partial_subscription_details())
    def test_round_trip(self, details):
        """
        A subscription created with a POST to /v1/subscriptions/ can be found in
        the list retrieved with a GET of /v1/subscriptions.
        """
        client = self.get_client()
        d = client.create(details.subscription_id, details)
        created = self.successResultOf(d)

        # Secrets are randomly assigned but we can scrape them out
        # of the created object.
        expected = attr.assoc(
            details,
            introducer_port_number=10000,
            storage_port_number=10001,
            oldsecrets=created.oldsecrets,
        )
        self.expectThat(expected, AttrsEquals(created))

        [listed] = self.successResultOf(client.list())
        self.expectThat(expected, AttrsEquals(listed))

        retrieved = self.successResultOf(client.get(details.subscription_id))
        self.expectThat(expected, AttrsEquals(retrieved))


    @given(
        subscription_id(),
        subscription_id(),
        partial_subscription_details(),
    )
    def test_resources_assigned(self, id_a, id_b, details):
        """
        Some empty fields of the subscription details ``POST`` are populated and
        included in the response.
        """
        assume(id_a != id_b)
        client = self.get_client()
        details_a = self.successResultOf(client.create(
            id_a, attr.assoc(details, subscription_id=id_a),
        ))
        details_b = self.successResultOf(client.create(
            id_b, attr.assoc(details, subscription_id=id_b),
        ))

        # Secrets get populated with some random goodness.
        self.expectThat(details_a.oldsecrets, Not(Is(None)))
        self.expectThat(details_b.oldsecrets, Not(Is(None)))
        self.expectThat(details_a.oldsecrets, Not(Equals(details_b.oldsecrets)))


    @given(partial_subscription_details())
    def test_deactivate_subscription(self, details):
        """
        A DELETE to /v1/subscriptions/<id> causes a subscription to be
        deactivated such that it is no longer included in the result
        of a GET to /v1/subscriptions.
        """
        client = self.get_client()
        self.successResultOf(client.create(details.subscription_id, details))
        self.successResultOf(client.delete(details.subscription_id))

        subscriptions = self.successResultOf(client.list())
        self.assertThat(subscriptions, Equals([]))



class SubscriptionManagerTests(SubscriptionManagerTestMixin, TestCase):
    def get_client(self):
        return memory_client(
            FilePath(mkdtemp().decode("utf-8")),
            u"s4.example.com"
        )


# TODO: A more integration-y test using network_client.


class MakeServiceTests(TestCase):
    def test_interface(self):
        """
        ``makeService`` returns an ``IService`` provider.
        """
        options = Options()
        options.parseOptions([
            b"--domain", b"s4.example.com",
            b"--state-path", self.mktemp(),
            b"--listen-address", b"tcp:12345",
        ])
        service = makeService(options)
        verifyObject(IService, service)
