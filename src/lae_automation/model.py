# Copyright Least Authority Enterprises.
# See LICENSE for details.

import attr
from attr import validators

from pem import parse

from twisted.python.url import URL

from foolscap.pb import Tub
from foolscap.furl import decode_furl, encode_furl

from lae_util.validators import all

validate_furl = all(
    attr.validators.instance_of(str),
    lambda inst, attr, value: decode_furl(value),
)



@attr.s(frozen=True)
class DeploymentConfiguration(object):
    domain = attr.ib(validator=attr.validators.instance_of(unicode))

    # This supposes the storage server and introducer run in the same pod.
    # Which is how they run for now.  And I'm not sure why it would ever be
    # different.
    private_host = u"127.0.0.1"

    # It is optional because DeploymentConfiguration is used basically
    # everywhere but only the subscription-converger needs to know a
    # Kubernetes namespace. :/
    kubernetes_namespace = attr.ib(
        validator=attr.validators.optional(attr.validators.instance_of(unicode)),
    )

    # It is optional for the same reason as above.
    subscription_manager_endpoint = attr.ib(
        validator=attr.validators.optional(attr.validators.instance_of(URL)),
    )

    # Tahoe-LAFS is configured with these keys, allowing it to talk to S3 and
    # store and retrieve shares.
    s3_access_key_id = attr.ib(validator=attr.validators.instance_of(unicode))
    s3_secret_key = attr.ib(repr=False, validator=attr.validators.instance_of(unicode))

    # Docker images to run per-subscription containers.
    introducer_image = attr.ib(validator=attr.validators.instance_of(unicode))
    storageserver_image = attr.ib(validator=attr.validators.instance_of(unicode))

    log_gatherer_furl = attr.ib(default=None, validator=attr.validators.optional(validate_furl))
    stats_gatherer_furl = attr.ib(default=None, validator=attr.validators.optional(validate_furl))


class NullDeploymentConfiguration(object):
    domain = None
    private_host = None
    kubernetes_namespace = None
    subscription_manager_endpoint = None
    s3_access_key_id = None
    s3_secret_key = None
    introducer_image = None
    storageserver_image = None

    log_gatherer_furl = None
    stats_gatherer_furl = None



def _parse(pem_str):
    return tuple(sorted(parse(pem_str)))



def _convert_oldsecrets(oldsecrets):
    if oldsecrets:
        oldsecrets = oldsecrets.copy()
        if oldsecrets["introducer_node_pem"] is not None:
            oldsecrets["introducer_node_pem"] = parse(oldsecrets["introducer_node_pem"])
        if oldsecrets["server_node_pem"] is not None:
            oldsecrets["server_node_pem"] = parse(oldsecrets["server_node_pem"])
        return oldsecrets
    return {}



def make_external_furl(internal_furl, publichost, port):
    tub_id, location_hints, name = decode_furl(internal_furl)
    location_hints[:] = [u"{}:{}".format(publichost, port).encode("ascii")]
    return encode_furl(tub_id, location_hints, name)



@attr.s(frozen=True)
class SubscriptionDetails(object):
    """
    All of the specifics of a single S4 subscription.

    :ivar unicode subscription_id: The S4 internal subscription identifier for
        this subscription.  This is unique across subscriptions in a
        particular S4 deployment.  It is also persistent.  It is the primary
        key for subscriptions.  For subscriptions this may be equal to
        ``subscription_id`` but this is not necessarily the case.

    :ivar unicode stripe_subscription_id: This is the Stripe subscription
        identifier for the Stripe subscription associated with this S4
        subscription.  This may change if certain billing-related events take
        place.
    """
    # TODO: Old subscriptions have distinctive bucket names.  Newer
    # subscriptions all share a bucket.  It would be nice to migrate all the
    # old subscription data to the single shared bucket and then get rid of
    # this field.  Once there's just one bucket, the configuration about which
    # bucket that is can live on DeploymentConfiguration.
    bucketname = attr.ib()

    # Like the thing returned by secrets_to_legacy_format.
    oldsecrets = attr.ib(
        convert=_convert_oldsecrets,
        validator=validators.instance_of(dict),
    )
    customer_email = attr.ib()
    customer_pgpinfo = attr.ib()

    product_id = attr.ib(
        validator=validators.instance_of(unicode),
    )

    # Referencing opaque stripe identifiers
    customer_id = attr.ib()
    subscription_id = attr.ib()

    introducer_port_number = attr.ib()
    storage_port_number = attr.ib()

    stripe_subscription_id = attr.ib(
        validator=validators.instance_of(unicode),
    )

    key_prefix = attr.ib(
        validator=validators.instance_of(unicode),
        default=u"",
    )

    @property
    def publichost(self):
        return self.oldsecrets["publichost"]

    @property
    def privatehost(self):
        return self.oldsecrets["privatehost"]

    @property
    def external_introducer_furl(self):
        return make_external_furl(
            self.oldsecrets["internal_introducer_furl"],
            self.publichost,
            self.introducer_port_number,
        )

    @property
    def introducer_node_pem(self):
        return "".join(map(str, self.oldsecrets["introducer_node_pem"]))

    @property
    def server_node_pem(self):
        return "".join(map(str, self.oldsecrets["server_node_pem"]))

    @property
    def introducer_tub_id(self):
        return Tub(self.introducer_node_pem).getTubID().decode("ascii")

    @property
    def storage_tub_id(self):
        return Tub(self.server_node_pem).getTubID().decode("ascii")
