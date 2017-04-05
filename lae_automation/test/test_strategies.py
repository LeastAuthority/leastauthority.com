# Copyright Least Authority Enterprises.
# See LICENSE for details.

from hypothesis import given

from testtools.matchers import Not, Equals, GreaterThan, LessThan

from lae_util.testtools import TestCase

from .strategies import two_distinct_ports


class TwoDistinctPorts(TestCase):
    @given(two_distinct_ports())
    def test_constraints(self, ports):
        a, b = ports
        self.expectThat(a, GreaterThan(0))
        self.expectThat(a, LessThan(65536))
        self.expectThat(b, GreaterThan(0))
        self.expectThat(b, LessThan(65536))
        self.expectThat(a, Not(Equals(b)))
