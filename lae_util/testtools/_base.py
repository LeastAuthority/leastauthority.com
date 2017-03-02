# Copyright 2014-2016 ClusterHQ
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Base classes for unit tests.

Originally ``flocker/testtools/_base.py``.
"""

import tempfile
from unittest import SkipTest

from testtools.content import text_content

from twisted.python import log
from twisted.python.filepath import FilePath
from twisted.trial import unittest

from txkube.testing import TestCase as _TestCase


def make_file(path, content='', permissions=None):
    """
    Create a file with given content and permissions.

    Don't use this for sensitive content, as the permissions are applied
    *after* the data is written to the filesystem.

    :param FilePath path: Path to create the file.
    :param str content: Content to write to the file. If not specified,
    :param int permissions: Unix file permissions to be passed to ``chmod``.

    :return: ``path``, unmodified.
    :rtype: :py:class:`twisted.python.filepath.FilePath`
    """
    path.setContent(content)
    if permissions is not None:
        path.chmod(permissions)
    return path


class _MktempMixin(object):
    """
    ``mktemp`` support for testtools TestCases.
    """

    def mktemp(self):
        """
        Create a temporary path for use in tests.

        Provided for compatibility with Twisted's ``TestCase``.

        :return: Path to non-existent file or directory.
        """
        return self.make_temporary_path().path

    def make_temporary_path(self):
        """
        Create a temporary path for use in tests.

        :return: Path to non-existent file or directory.
        :rtype: FilePath
        """
        return self.make_temporary_directory().child('temp')

    def make_temporary_directory(self):
        """
        Create a temporary directory for use in tests.

        :return: Path to directory.
        :rtype: FilePath
        """
        return make_temporary_directory(_path_for_test(self))

    def make_temporary_file(self, content='', permissions=None):
        """
        Create a temporary file for use in tests.

        :param str content: Content to write to the file.
        :param int permissions: The permissions for the file
        :return: Path to file.
        :rtype: FilePath
        """
        return make_file(self.make_temporary_path(), content, permissions)


class _DeferredAssertionMixin(object):
    """
    Synchronous Deferred-related assertions support for testtools TestCase.

    This is provided for compatibility with Twisted's TestCase.  New code
    should use matchers instead.
    """
    successResultOf = unittest.SynchronousTestCase.successResultOf.__func__
    failureResultOf = unittest.SynchronousTestCase.failureResultOf.__func__
    assertNoResult = unittest.SynchronousTestCase.assertNoResult.__func__

    # Not related to Deferreds but required by the implementation of the above.
    assertIdentical = unittest.SynchronousTestCase.assertIdentical.__func__


class TestCase(_TestCase, _MktempMixin, _DeferredAssertionMixin):
    """
    Base class for synchronous test cases.
    """
    # Eliot's validateLogging hard-codes a check for SkipTest when deciding
    # whether to check for valid logging, which is fair enough, since there's
    # no other API for checking whether a test has skipped. Setting
    # skipException tells testtools to treat unittest.SkipTest as the
    # exception that signals skipping.
    skipException = SkipTest

    def setUp(self):
        log.msg("--> Begin: %s <--" % (self.id()))
        super(TestCase, self).setUp()



def _test_skipped(case, result, exception):
    result.addSkip(case, details={'reason': text_content(unicode(exception))})


def _path_for_test_id(test_id, max_segment_length=32):
    """
    Get the temporary directory path for a test ID.

    :param str test_id: A fully-qualified Python name. Must
        have at least three components.
    :param int max_segment_length: The longest that a path segment may be.
    :return: A relative path to ``$module/$class/$method``.
    """
    if test_id.count('.') < 2:
        raise ValueError(
            "Must have at least three components (e.g. foo.bar.baz), got: %r"
            % (test_id,))
    return '/'.join(
        segment[:max_segment_length] for segment in test_id.rsplit('.', 2))


def _path_for_test(test):
    """
    Get the temporary directory path for a test.
    """
    return FilePath(_path_for_test_id(test.id()))


def make_temporary_directory(base_path):
    """
    Create a temporary directory beneath ``base_path``.

    It is the responsibility of the caller to delete the temporary directory.

    :param FilePath base_path: Base directory for the temporary directory.
        Will be created if it does not exist.
    :return: The FilePath to a newly-created temporary directory, created
        beneath the current working directory.
    """
    if not base_path.exists():
        base_path.makedirs()
    temp_dir = tempfile.mkdtemp(dir=base_path.path)
    return FilePath(temp_dir)
