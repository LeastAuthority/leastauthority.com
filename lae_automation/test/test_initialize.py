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
from lae_automation.initialize import (
    verify_and_store_serverssh_pubkey, PublicKeyMismatch,
    _get_entry_from_keyscan, create_user_bucket,
)

from .strategies import aws_access_key_id, aws_secret_key, bucket_name

ACTIVATIONKEY= 'MOCKACTIVATONKEY'
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
USERTOKEN = 'TESTUSERTOKEN'+'A'*385


class InitializationTests(AsyncTestCase):
    def _patch_mock(self, obj, attribute):
        m = mock.Mock()
        self.patch(obj, attribute, m)
        return m

    def setUp(self):
        self.mockwaitfor_console = self._patch_mock(lae_automation.initialize,
                                                    'wait_for_EC2_sshfp')
        self.mockwaitfor_keyscan = self._patch_mock(lae_automation.initialize,
                                                    'wait_for_and_store_pubkeyfp_from_keyscan')
        self.mocklsc = self._patch_mock(lae_automation.initialize, 'LicenseServiceClient')

    def test_verify_and_store_serverssh_pubkey(self):
        self.mockwaitfor_console.side_effect = lambda *args: defer.succeed(
            'NONMATCHING FINGERPRINT_FROM_CONSOLE')
        self.mockwaitfor_keyscan.side_effect = lambda *args: defer.succeed(
            ('NONMATCHING FINGERPRINT_FROM_KEYSCAN', 'HASHED_PUBKEY') )

        mismatchfailure = Failure(PublicKeyMismatch)
        argtuple = ('ec2accesskeyid', 'ec2secretkey', 'endpoint_uri', 'pub_ipaddress', .1, 600,
                    sys.stdout, sys.stderr, 'instance_id')
        d = self.failUnlessFailure(verify_and_store_serverssh_pubkey(*argtuple),
                                   mismatchfailure.value)
        return d


class KeyscanTests(TestCase):
    """
    Tests for ``_get_entry_from_keyscan``.
    """
    # Dummy key, randomly generated using `ssh-keygen -t rsa -b 1024`
    # on Ubuntu 16.10.
    RSA_KEY = Key.fromString("""\
-----BEGIN RSA PRIVATE KEY-----
MIICXAIBAAKBgQDPO2Rzfbky7sh1gbmN4+GIRkg+fM6dZEhlJsQqjbBsH9i2tuOc
kEgkfCpxayYtzvm5vOzOkhenGqjp7UQuYxxNhMKGxScMlnvpzPTChGoaWxTzmK8X
yaT55rh8EYZIx5EIBm8tfsYJUbQFgNnGSRIKluMTpW8+CqO9uU0V5olkGQIDAQAB
AoGAAMPlYQ/LyUZccyKhfsaipJAt4B0x3h7qrYTxIH8ZcazEbhhKyt81hPz4YybU
I0MqZOcvsKuVbsaIbSS1Jb6z8guf9mJDEORhBZfAzKkaj5wZwA997slMLl3W9NRL
awyse3Xj6SHyqwUsDbnYPwTHqOsurRqnKHeop3MRXZbSeSkCQQD9pT0dSnK5o6DG
eFP9SnDl/bZ0+9ASaJpkxbkopHmSUAQ517Yu4eDZkGaqCf8BY4WJrUggK0wBL874
Co62iu1nAkEA0SfbZtthU2pMb9szY/rwsQHpUCMeN0Uw8K5Z6rxrrDqGlCIFNra0
RleQIwGkV+O7K0IvqxIZXwKgxKEKKZSyfwJBAIWOGBvwM3BkJCfc+/yG0eOIMCZw
4SKQSZt+MPyhfhH4aAE9AAS3kvl7+1LVaJyGlq3ju/KUWbTWQ5h/lp2vkUkCQG23
tc1oKc8DRSOsXnIFMnv4X7btJS2jO0AWhg6wVt9bODu++PMxtrHrvy3N77M3QHk5
2B2qeeqwSzu6qsUTPusCQDnwU5NZWJMqWpnKd8IULXaK6pIKpJtgdNK7E4xCVYHA
yxxXP/Crot2XznHTZLWsSdgRzc0GuLysclbOo6jIcw4=
-----END RSA PRIVATE KEY-----
""")

    def test_empty(self):
        """
        If the given file contains no entries, ``None`` is returned.
        """
        path = FilePath(self.mktemp())
        path.makedirs()
        empty = path.child(b"empty_known_hosts")
        empty.touch()
        self.assertIs(None, _get_entry_from_keyscan(empty))

    def test_rsakey(self):
        """
        If the given file contains a n RSA key entry, its MD5 fingerprint
        is returned along with the entry itself.
        """
        path = FilePath(self.mktemp())
        path.makedirs()
        rsa_path = path.child(b"rsa_known_hosts")
        known_hosts = KnownHostsFile.fromPath(rsa_path)
        known_hosts.addHostKey(b"example.com", self.RSA_KEY)
        known_hosts.save()

        self.assertEqual(
            (self.RSA_KEY.fingerprint(), rsa_path.getContent().strip()),
            _get_entry_from_keyscan(rsa_path)
        )


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
