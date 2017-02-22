
import mock
from hypothesis import given, settings

from os import fdopen
from json import loads, dumps
from cStringIO import StringIO
from tempfile import mkstemp

from twisted.python.filepath import FilePath
from twisted.python.url import URL

from datetime import datetime

from testtools.matchers import Equals

from lae_util.testtools import TestCase

from lae_automation import server
from lae_automation.model import DeploymentConfiguration
from lae_automation.server import api

from .matchers import hasLocationHint
from .strategies import (
    bucket_name, ipv4_address, aws_access_key_id, aws_secret_key,
    furl,
)

INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

# Vector data for the config file data:
from lae_automation.test.test_vectors import MOCKJSONCONFIGFILE
CONFIGFILEJSON = MOCKJSONCONFIGFILE
def fifo(xs):
    xs.reverse()
    return xs


class CommonTestFixture(TestCase):
    def setUp(self):
        super(CommonTestFixture, self).setUp()
        self.TEST_SHA1_STRING = 'deadbeef'*5
        self.TEST_IPv4_STRING = '0.0.0.0'
        self.TEST_LOCAL_HOST_STRING = self.TEST_IPv4_STRING
        self.TEST_LOCAL_REPO_STRING = './.git'
        self.TEST_ADMIN_PRIVKEY_PATH_STRING = '../secret_config/PRIVKEYFILE.pem'
        self.TEST_GIT_SSH_PATH_STRING = 'PATHTOLADIR/leastauthority.com/git_ssh.sh'
        self.TEST_LIVE_PATH_STRING = '/home/website/SECRET_OR_LA_DIR/.git'
        self.TEST_DATETIME = datetime(2014, 5, 19, 14, 32, 31, 788921)
        self.TEST_TAG_NAME = '2014-05-19T14-32-31Z_0.0.0.0_deadbeef'



class NewTahoeConfigurationTests(TestCase):
    """
    Tests for ``new_tahoe_configuration``.
    """
    @given(
        bucket_name(),
        ipv4_address(), ipv4_address(),
        aws_access_key_id(), aws_secret_key(),
        furl(), furl(),
    )
    # Limit the number of iterations of this test - generating RSA
    # keys is slow.
    @settings(max_examples=10)
    def test_generated(
            self,
            bucket_name,
            publichost, privatehost,
            s3_access_key_id, s3_secret_key,
            log_gatherer_furl, stats_gatherer_furl,
    ):
        """
        New introducer and storage configuration can be created with
        ``new_tahoe_configuration``.
        """
        deploy_config = DeploymentConfiguration(
            domain=u"testing.com",
            kubernetes_namespace=u"testing",
            subscription_manager_endpoint=URL.fromText(u"http://localhost/"),
            products=[{}],
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,

            introducer_image=u"tahoe-introducer",
            storageserver_image=u"tahoe-storageserver",

            secretsfile=fdopen(mkstemp()[0], "w"),
            serverinfopath=None,

            ssec2_access_key_id=None,
            ssec2_secret_path=None,

            ssec2admin_keypair_name=None,
            ssec2admin_privkey_path=None,

            monitor_pubkey_path=None,
            monitor_privkey_path=None,

            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        )
        config = server.new_tahoe_configuration(
            deploy_config, bucket_name, publichost, privatehost, 4321, 1234,
        )
        # It returns an object which can be round-tripped through the
        # JSON format.
        self.expectThat(config, Equals(loads(dumps(config))))

        # The introducer and storage are both told the same introducer
        # furl.
        self.expectThat(
            config["introducer"]["introducer_furl"],
            Equals(config["storage"]["introducer_furl"]),
        )

        # And ports are what we said.
        self.expectThat(config["introducer"]["port"], Equals(4321))
        self.expectThat(config["storage"]["port"], Equals(1234))

        # The introducer furl is contains a location hint of the
        # public host and the hard-coded introducer port we use.
        self.expectThat(
            config["introducer"]["introducer_furl"],
            hasLocationHint(config["storage"]["publichost"], 4321),
        )
