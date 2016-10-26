import sys

from twisted.trial.unittest import TestCase
from twisted.internet import defer
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath

from twisted.conch.ssh.keys import Key
from twisted.conch.client.knownhosts import KnownHostsFile

import mock

import lae_automation.initialize
from lae_automation.initialize import (
    verify_and_store_serverssh_pubkey, PublicKeyMismatch,
    _get_entry_from_keyscan,
)


ACTIVATIONKEY= 'MOCKACTIVATONKEY'
PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
USERTOKEN = 'TESTUSERTOKEN'+'A'*385


class InitializationTests (TestCase):
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
