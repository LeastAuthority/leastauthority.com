# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_automation.containers``.
"""

from __future__ import unicode_literals

from json import loads

from hypothesis import given

from foolscap.furl import decode_furl

from testtools.matchers import Equals, Contains, Not

from txkube import v1_5_model as model

from lae_util.testtools import TestCase

from .strategies import deployment_configurations, subscription_details

from ..containers import (
    create_configuration,
    create_deployment,
)


class CreateConfigurationTests(TestCase):
    """
    Tests for ``create_configuration``.
    """
    @given(deployment_configurations(), subscription_details())
    def test_introducer_furl(self, deploy_config, details):
        """
        The introducer furl included in the generated configuration includes the
        introducer tub id and swissnum and a connection hint derived from the
        address information on the subscription details.
        """
        config = create_configuration(deploy_config, details, model)
        introducer_furl = loads(config.data["introducer.json"])["introducer"]["introducer_furl"]
        self.assertThat(
            decode_furl(introducer_furl),
            Equals((
                details.introducer_tub_id,
                ["{}:{}".format(details.publichost, details.introducer_port_number)],
                decode_furl(details.external_introducer_furl)[2],
            )),
        )
        # There's another copy of that in the storage server's config.  Make
        # sure it matches.
        self.assertThat(
            introducer_furl,
            Equals(loads(config.data["storage.json"])["storage"]["introducer_furl"]),
        )


class CreateDeploymentTests(TestCase):
    """
    Tests for ``create_deployment``.
    """
    @given(deployment_configurations(), subscription_details())
    def test_image(self, deploy_config, details):
        """
        Each container in the created deployment has an image specified.
        """
        deployment = create_deployment(
            deploy_config,
            details,
            model,
        )
        for container in deployment.spec.template.spec.containers:
            self.assertThat((None, u""), Not(Contains(container.image)))
