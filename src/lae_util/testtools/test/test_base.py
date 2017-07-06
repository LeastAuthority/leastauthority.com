# Copyright ClusterHQ Inc.  See LICENSE file for details.

"""
Tests for flocker base test cases.
"""

import errno
import os
import shutil
import unittest

from fixtures import Fixture, MonkeyPatch
from hypothesis import assume, given
from hypothesis.strategies import binary, integers, text

# Use testtools' TestCase for most of these tests so that bugs in our base test
# case classes don't invalidate the tests for those classes.
from testtools import TestCase as TesttoolsTestCase
from testtools import TestResult
from testtools.matchers import (
    AllMatch,
    AfterPreprocessing,
    Annotate,
    Contains,
    DirExists,
    HasLength,
    Equals,
    FileContains,
    Matcher,
    MatchesAny,
    LessThan,
    Not,
    PathExists,
    StartsWith,
    IsInstance,
)
from twisted.internet.defer import Deferred, succeed, fail
from twisted.python.filepath import FilePath
from twisted.python.failure import Failure

from .. import CustomException, TestCase
from .._base import (
    _path_for_test_id,
)
from ..matchers import dir_exists, file_contents
from ..strategies import fqpns
from .._testhelpers import (
    base_test_cases,
    has_results,
    make_test_case,
    only_skips,
    run_test,
)


class BaseTestCaseTests(TesttoolsTestCase):
    """
    Tests for our base test cases.
    """

    @given(base_test_cases, text(average_size=30))
    def test_trial_skip_exception(self, base_test_case, reason):
        """
        If tests raise the ``SkipTest`` exported by Trial, then that's
        recorded as a skip.
        """

        class SkippingTest(base_test_case):
            def test_skip(self):
                raise unittest.SkipTest(reason)

        test = SkippingTest('test_skip')
        result = run_test(test)
        self.assertThat(result, only_skips(1, [reason]))

    @given(base_test_cases.map(make_test_case))
    def test_mktemp_doesnt_exist(self, test):
        """
        ``mktemp`` returns a path that doesn't exist inside a directory that
        does.
        """
        temp_path = FilePath(test.mktemp())
        self.addCleanup(_remove_dir, temp_path.parent())

        self.expectThat(temp_path.parent().path, DirExists())
        self.expectThat(temp_path.path, Not(PathExists()))
        self.assertThat(temp_path, BelowPath(FilePath(os.getcwd())))

    @given(base_test_cases)
    def test_mktemp_not_deleted(self, base_test_case):
        """
        ``mktemp`` returns a path that's not deleted after the test is run.
        """
        created_files = []

        class SomeTest(base_test_case):
            def test_create_file(self):
                path = self.mktemp()
                created_files.append(path)
                open(path, 'w').write('hello')

        run_test(SomeTest('test_create_file'))
        [path] = created_files
        self.addCleanup(os.unlink, path)
        self.assertThat(path, FileContains('hello'))

    @given(base_test_cases.map(make_test_case))
    def test_make_temporary_path_doesnt_exist(self, test):
        """
        ``make_temporary_path`` returns a path that doesn't exist inside a
        directory that does.
        """
        temp_path = test.make_temporary_path()
        self.addCleanup(_remove_dir, temp_path.parent())

        self.expectThat(temp_path.parent().path, DirExists())
        self.expectThat(temp_path.path, Not(PathExists()))
        self.assertThat(temp_path, BelowPath(FilePath(os.getcwd())))

    @given(base_test_cases)
    def test_make_temporary_path_not_deleted(self, base_test_case):
        """
        ``make_temporary_path`` returns a path that's not deleted after the
        test is run.
        """
        created_files = []

        class SomeTest(base_test_case):
            def test_create_file(self):
                path = self.make_temporary_path()
                created_files.append(path)
                path.setContent('hello')

        run_test(SomeTest('test_create_file'))
        [path] = created_files
        self.addCleanup(path.remove)
        self.assertThat(path.path, FileContains('hello'))

    @given(base_test_cases.map(make_test_case))
    def test_make_temporary_directory_exists(self, test):
        """
        ``make_temporary_directory`` returns a path to a directory.
        """
        temp_dir = test.make_temporary_directory()
        self.addCleanup(_remove_dir, temp_dir)
        self.assertThat(temp_dir, dir_exists())

    @given(base_test_cases.map(make_test_case), binary(average_size=20))
    def test_make_temporary_file_with_content(self, test, content):
        """
        ``make_temporary_file`` returns a path to an existing file.
        """
        temp_file = test.make_temporary_file(content)
        self.addCleanup(temp_file.remove)
        self.assertThat(temp_file, file_contents(Equals(content)))

    @given(base_test_cases.map(make_test_case))
    def test_make_temporary_file(self, test):
        """
        ``make_temporary_file`` returns a path to an existing file. If no
        content is provided, defaults to an empty file.
        """
        temp_file = test.make_temporary_file()
        self.addCleanup(temp_file.remove)
        self.assertThat(temp_file, file_contents(Equals('')))

    @given(base_test_cases.map(make_test_case))
    def test_run_twice(self, test):
        """
        Tests can be run twice without errors.

        This is being fixed upstream at
        https://github.com/testing-cabal/testtools/pull/165/, and this test
        coverage is inadequate for a thorough fix. However, this will be
        enough to let us use ``trial -u`` (see FLOC-3462).
        """
        result = TestResult()
        test.run(result)
        test.run(result)
        self.assertThat(
            result, has_results(
                tests_run=Equals(2),
            )
        )


class DebugTwisted(Fixture):
    """
    Set debugging for various Twisted things.
    """

    def __init__(self, debug):
        """
        Set debugging for Deferreds and DelayedCalls.

        :param bool debug: If True, enable debugging. If False, disable it.
        """
        super(DebugTwisted, self).__init__()
        self._debug_setting = debug

    def _setUp(self):
        self.useFixture(
            MonkeyPatch('twisted.internet.defer.Deferred.debug',
                        self._debug_setting))
        self.useFixture(
            MonkeyPatch('twisted.internet.base.DelayedCall.debug',
                        self._debug_setting))


def match_text_content(matcher):
    """
    Match the text of a ``Content`` instance.
    """
    return AfterPreprocessing(lambda content: content.as_text(), matcher)



class MakeTemporaryTests(TesttoolsTestCase):
    """
    Tests for code for making temporary files and directories for tests.
    """

    @given(test_id=fqpns, max_length=integers(min_value=1, max_value=64))
    def test_directory_for_test(self, test_id, max_length):
        """
        _path_for_test_id returns a relative path of $module/$class/$method for
        the given test id.
        """
        assume(test_id.count('.') > 1)
        path = _path_for_test_id(test_id, max_length)
        self.expectThat(path, Not(StartsWith('/')))
        segments = path.split('/')
        self.expectThat(segments, HasLength(3))
        self.assertThat(
            segments,
            AllMatch(
                AfterPreprocessing(
                    len, MatchesAny(
                        LessThan(max_length),
                        Equals(max_length)
                    )
                )
            )
        )

    @given(test_id=fqpns)
    def test_too_short_test_id(self, test_id):
        """
        If the given test id is has too few segments, raise an error.
        """
        assume(test_id.count('.') < 2)
        self.assertRaises(ValueError, _path_for_test_id, test_id)


def _remove_dir(path):
    """
    Safely remove the directory 'path'.
    """
    try:
        shutil.rmtree(path.path)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise


class BelowPath(Matcher):
    """
    Match if the given path is a child (or grandchild, etc.) of the specified
    parent.
    """

    def __init__(self, parent):
        """
        Construct a ``BelowPath`` that will successfully match for any child
        of ``parent``.

        :param FilePath parent: The parent path. Any path beneath this will
            match.
        """
        self._parent = parent

    def match(self, child):
        """
        Assert ``child`` is beneath the core path.
        """
        return Annotate(
            "%s in not beneath %s" % (child, self._parent),
            Contains(self._parent)).match(child.parents())


class ResultOfAssertionsTests(TestCase):
    """
    Bare minimum of testing for the Deferred result-related assertions borrowed
    from Twisted.

    These tests aren't more thorough because we don't implement them in
    Flocker.  We just want to verify we've glued in the Twisted implementation
    successfully.
    """
    def test_assertNoResult(self):
        """
        ``flocker.testtools.TestCase.assertNoResult`` works like
        ``twisted.trial.unittest.SynchronousTestCase.assertNoResult``.
        """
        self.assertNoResult(Deferred())

    def test_successResultOf(self):
        """
        ``flocker.testtools.TestCase.successResultOf`` works like
        ``twisted.trial.unittest.SynchronousTestCase.successResultOf``.
        """
        self.assertThat(
            self.successResultOf(succeed(3)),
            Equals(3),
        )

    def test_failureResultOf(self):
        """
        ``flocker.testtools.TestCase.failureResultOf`` works like
        ``twisted.trial.unittest.SynchronousTestCase.failureResultOf``.
        """
        self.assertThat(
            self.failureResultOf(fail(CustomException()), CustomException),
            IsInstance(Failure),
        )
