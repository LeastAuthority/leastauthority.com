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
from .matchers import AttrsEquals, GoodEquals


def partial_subscription_details():
    return subscription_details().map(
        lambda d: attr.assoc(
            d,
            bucketname=None,
            # Duplicate key prefix assignment logic in subscription manager. :/
            key_prefix=d.subscription_id + u"/",
            oldsecrets=None,
        ),
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
        A subscription created with ``create`` can be found in the list retrieved
        with ``list``.
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

            # This isn't random but it's hard to predict the value from here.
            # Just make sure we get the same values back from subsequent
            # calls.
            bucketname=created.bucketname,
        )
        self.expectThat(
            expected, AttrsEquals(created),
            "create() subscription mismatches expected",
        )

        [listed] = self.successResultOf(client.list())
        self.expectThat(
            expected, AttrsEquals(listed),
            "list() subscription mismatches expected",
        )

        retrieved = self.successResultOf(client.get(details.subscription_id))
        self.expectThat(
            expected, AttrsEquals(retrieved),
            "get() subscription mismatches expected",
        )


    @given(
        subscription_id(),
        subscription_id(),
        partial_subscription_details(),
    )
    def test_resources_assigned(self, id_a, id_b, details):
        """
        Some empty fields of the subscription details given to ``create`` are
        populated and included in the result.
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
        ``delete`` causes a subscription to be deactivated such that it is no
        longer included in the result of ``list``.
        """
        client = self.get_client()
        self.successResultOf(client.create(details.subscription_id, details))
        self.successResultOf(client.delete(details.subscription_id))

        subscriptions = self.successResultOf(client.list())
        self.assertThat(subscriptions, Equals([]))


    @given(subscription_details())
    def test_load_subscription(self, details):
        """
        ``load`` causes a subscription to be created exactly as specified by the
        given details - with no secret generation.
        """
        client = self.get_client()
        self.successResultOf(client.load(details))
        [subscription] = self.successResultOf(client.list())
        self.assertThat(
            # The ports don't matter, the server still gets to assign them.
            attr.assoc(
                subscription,
                introducer_port_number=details.introducer_port_number,
                storage_port_number=details.storage_port_number,
            ),
            GoodEquals(details),
        )



class SubscriptionManagerTests(SubscriptionManagerTestMixin, TestCase):
    def get_client(self):
        return memory_client(
            FilePath(mkdtemp().decode("utf-8")),
            u"s4.example.com",
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
            b"--bucket-name", b"s4-bucket",
            b"--state-path", self.mktemp(),
            b"--listen-address", b"tcp:12345",
        ])
        service = makeService(options)
        verifyObject(IService, service)
