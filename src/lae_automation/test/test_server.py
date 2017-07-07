# Copyright Least Authority Enterprises.
# See LICENSE for details.

from hypothesis import given, settings

from os import fdopen
from json import loads, dumps
from tempfile import mkstemp

from twisted.python.url import URL

from testtools.matchers import Equals

from lae_util.testtools import TestCase

from lae_automation import server
from lae_automation.model import DeploymentConfiguration

from .matchers import hasLocationHint
from .strategies import (
    bucket_name, ipv4_address, aws_access_key_id, aws_secret_key,
    furl,
)

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
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,

            introducer_image=u"tahoe-introducer",
            storageserver_image=u"tahoe-storageserver",

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
