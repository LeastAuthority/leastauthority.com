# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module implements Hamcrest-style matchers for testtools-based
tests.
"""

import operator
from ConfigParser import SafeConfigParser

import attr

from testtools.matchers import Mismatch, AfterPreprocessing, Contains, Equals

from foolscap.furl import decode_furl

def hasLocationHint(host, port):
    """
    Match a Foolscap furl which includes the given location hint.
    """
    def _get_hints(furl):
        tub_id, hints, name = decode_furl(furl)
        return hints
    return AfterPreprocessing(
        _get_hints,
        Contains("{}:{}".format(host, port)),
    )

def hasContents(contents):
    """
    Match a FilePath referring to a file containing exactly the given
    contents.
    """
    def _readPath(path):
        return path.getContent()
    return AfterPreprocessing(_readPath, Equals(contents))


def hasConfiguration(fields):
    """
    Match a FilePath referring to a file containing an ini-style
    configuration with at least the given fields.
    """
    expected_fields = sorted(fields)

    def _readConfig(config_path):
        config = SafeConfigParser()
        config.readfp(config_path.open())
        actual_fields = sorted(
            (section, field, config.get(section, field))
            for (section, field, _)
            in expected_fields
        )
        return actual_fields
    return AfterPreprocessing(_readConfig, Equals(expected_fields))


@attr.s(frozen=True)
class AttrsEquals(object):
    expected = attr.ib()

    comparator = operator.eq
    mismatch_string = "!="

    def __str__(self):
        return "%s(%r)" % (self.__class__.__name__, self.expected)

    def match(self, other):
        if self.comparator(other, self.expected):
            return None
        return _AttrsMismatch(other, self.mismatch_string, self.expected)


class _AttrsMismatch(Mismatch):
    def __init__(self, actual, mismatch_string, reference,
                 reference_on_right=True):
        self._actual = actual
        self._mismatch_string = mismatch_string
        self._reference = reference
        self._reference_on_right = reference_on_right

    def describe(self):
        if type(self._actual) != type(self._reference):
            return (
                "type mismatch:\n"
                "reference = %s\n"
                "actual = %s\n"
            ) % (
                type(self._reference),
                type(self._actual),
            )

        mismatched = []
        fields = attr.fields(type(self._actual))
        for field in fields:
            actual = getattr(self._actual, field.name)
            reference = getattr(self._reference, field.name)
            if actual != reference:
                mismatched.append((field.name, actual, reference))

        return "field mismatch:\n" + "".join(
            "field: %s\nreference = %s\nactual = %s\n" % mismatch
            for mismatch
            in mismatched
        )
