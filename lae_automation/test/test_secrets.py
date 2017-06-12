# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for secrets-management related functionality.
"""

from __future__ import unicode_literals

from yaml import safe_load

from testtools.matchers import AfterPreprocessing, Equals

from lae_util.testtools import TestCase

from filepath import FilePath

from lae_automation import __file__ as lae_package_location

ROOT = FilePath(lae_package_location).parent().parent()
STAGING = ROOT.child("k8s").child("secrets.staging.enc.yaml")
PRODUCTION = ROOT.child("k8s").child("secrets.production.enc.yaml")


class SecretsFileTests(TestCase):
    """
    Tests for the files in the repository which contain the S4 secrets.
    """
    def test_same_staging_and_production_structure(self):
        """
        The staging and production secrets files have the same overall structure
        (the same keys and hierarchy).
        """
        with STAGING.open() as staging, PRODUCTION.open() as production:
            staging_keys = list(hierarchical_keys(safe_load(staging)))
            self.assertThat(
                safe_load(production),
                has_keys(staging_keys),
            )



def has_keys(keys):
    return AfterPreprocessing(
        lambda o: list(hierarchical_keys(o)),
        Equals(keys),
    )




def hierarchical_keys(obj):
    if isinstance(obj, dict):
        for k, v in obj.items():
            if isinstance(v, dict):
                for sub_k in hierarchical_keys(v):
                    yield (k,) + sub_k
            else:
                yield (k,)



class HierarchicalKeysTests(TestCase):
    def test_result(self):
        self.expectThat(None, has_keys([]))
        self.expectThat("", has_keys([]))
        self.expectThat({}, has_keys([]))
        self.expectThat(
            {"foo": "bar"},
            has_keys([("foo",)]),
        )
        self.expectThat(
            {"foo": "bar", "baz": "quux"},
            has_keys([("foo",), ("baz",)]),
        )
        self.expectThat(
            {"foo": "bar", "baz": {"a": 1, "b": 2}},
            has_keys([("foo",), ("baz", "a"), ("baz", "b")]),
        )
