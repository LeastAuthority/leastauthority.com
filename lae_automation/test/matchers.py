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
class GoodEquals(object):
    expected = attr.ib()

    def match(self, other):
        if other == self.expected:
            return None
        return _DiffMismatch(other, u"!=", self.expected)


@attr.s(frozen=True)
class _DiffMismatch(object):
    actual = attr.ib()
    mismatch_string = attr.ib()
    reference = attr.ib()

    def describe(self):
        from deepdiff import DeepDiff
        from pprint import pformat
        return u"(actual = old; reference = new)\n" + pformat(
            DeepDiff(self.actual, self.reference, view="text")
        )




@attr.s(frozen=True)
class MappingLikeEquals(object):
    expected = attr.ib()

    comparator = operator.eq
    mismatch_string = "!="

    def fields(self, obj):
        raise NotImplementedError()

    def get_field(self, obj, field):
        raise NotImplementedError()

    def __str__(self):
        return "%s(%r)" % (self.__class__.__name__, self.expected)

    def match(self, other):
        if self.comparator(other, self.expected):
            return None
        return _MappingLikeMismatch(self.fields, self.get_field, other, self.mismatch_string, self.expected)

@attr.s(frozen=True)
class MappingEquals(MappingLikeEquals):
    def fields(self, obj):
        return obj.keys()

    def get_field(self, obj, key):
        return obj[key]

@attr.s(frozen=True)
class AttrsEquals(MappingLikeEquals):
    def fields(self, obj):
        return list(field.name for field in attr.fields(type(obj)))

    def get_field(self, obj, key):
        return getattr(obj, key)

class _MappingLikeMismatch(Mismatch):
    def __init__(self, get_fields, get_field, actual, mismatch_string, reference,
                 reference_on_right=True):
        self._get_fields = get_fields
        self._get_field = get_field
        self._actual = actual
        self._mismatch_string = mismatch_string
        self._reference = reference
        self._reference_on_right = reference_on_right

    def describe(self):
        if type(self._actual) != type(self._reference):
            return (
                "type mismatch:\n"
                "reference = %s\n"
                "actual    = %s\n"
            ) % (
                type(self._reference),
                type(self._actual),
            )

        mismatched = []
        fields = self._get_fields(self._actual)
        for field in sorted(fields):
            actual = self._get_field(self._actual, field)
            try:
                reference = self._get_field(self._reference, field)
            except KeyError:
                mismatched.append((field, actual, "<<missing>>"))
            else:
                if actual != reference:
                    mismatched.append((field, actual, reference))

        extra = set(self._get_fields(self._reference)) - set(fields)
        for field in sorted(extra):
            reference = self._get_field(self._reference, field)
            mismatched.append((field, "<<missing>>", reference))

        return "field mismatch:\n" + "".join(
            "field: %s\n"
            "reference = %s\n"
            "actual    = %s\n" % mismatch
            for mismatch
            in mismatched
        )
