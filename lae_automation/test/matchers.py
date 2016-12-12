# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module implements Hamcrest-style matchers for testtools-based
tests.
"""

from ConfigParser import SafeConfigParser

from testtools.matchers import AfterPreprocessing, Contains, Equals

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
