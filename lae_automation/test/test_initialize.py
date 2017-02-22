# Copyright Least Authority Enterprises.
# See LICENSE for details.

import sys

from twisted.trial.unittest import TestCase as AsyncTestCase
from twisted.internet import defer
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath
from twisted.internet.task import Clock

from twisted.conch.ssh.keys import Key
from twisted.conch.client.knownhosts import KnownHostsFile

from hypothesis import given

import mock

from testtools.matchers import Is

from txaws.testing.s3 import MemoryS3
from txaws.credentials import AWSCredentials

from lae_util.testtools import TestCase

import lae_automation.initialize
from lae_automation.initialize import create_user_bucket

from .strategies import aws_access_key_id, aws_secret_key, bucket_name

ACTIVATIONKEY= 'MOCKACTIVATONKEY'
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
USERTOKEN = 'TESTUSERTOKEN'+'A'*385


class CreateUserBucketTests(TestCase):
    """
    Tests for ``create_user_bucket``.
    """
    @given(aws_access_key_id(), aws_secret_key(), bucket_name())
    def test_retry_on_failure(self, access_key_id, secret_key, bucket_name):
        """
        If bucket creation fails with an S3 error, the creation attempt is
        retried after a delay.
        """
        reactor = Clock()

        controller = MemoryS3()
        creds = AWSCredentials(access_key_id, secret_key)
        client, state = controller.client(creds=creds, endpoint=None)
        state.set_rate_limit_exceeded()

        d = create_user_bucket(reactor, client, bucket_name)
        # Let several minutes pass (in one second increments) while
        # the rate limit error is in place.
        reactor.pump([1] * 60 * 3)

        # It should still be retrying.
        self.assertNoResult(d)

        # Clear the rate limit error and let it try again.
        state.clear_rate_limit_exceeded()
        reactor.pump([1] * 60)

        # It should have met with success at this point.
        self.assertThat(self.successResultOf(d), Is(None))
