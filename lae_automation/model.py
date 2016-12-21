import attr
from pyrsistent import freeze

from foolscap.furl import decode_furl

from lae_util.validators import all

def _validate_products(instance, attribute, value):
    if len(value) == 0:
        raise ValueError("At least one product is required.")

validate_furl = all(
    attr.validators.instance_of(str),
    lambda inst, attr, value: decode_furl(value),
)

@attr.s(frozen=True)
class DeploymentConfiguration(object):
    # XXX Would be nice to be able to set this to
    # staging.leastauthority.com sometimes.
    domain = "leastauthority.com"

    # XXX Sets up internal DNS, must agree with other bits of the
    # system
    private_host = "customer-grid-service"

    products = attr.ib(validator=_validate_products)
    s3_access_key_id = attr.ib()
    s3_secret_key = attr.ib(repr=False)
    amiimageid = attr.ib()
    instancesize = attr.ib()

    # DevPay configuration.  Just for TLoS3, probably obsolete.
    usertoken = attr.ib()
    producttoken = attr.ib()

    ssec2_access_key_id = attr.ib()
    ssec2_secret_path = attr.ib()

    ssec2admin_keypair_name = attr.ib()
    ssec2admin_privkey_path = attr.ib()

    monitor_pubkey_path = attr.ib()
    monitor_privkey_path = attr.ib()

    secretsfile = attr.ib(validator=attr.validators.instance_of(file))
    serverinfopath = attr.ib(default="../serverinfo.csv")

    log_gatherer_furl = attr.ib(default=None, validator=attr.validators.optional(validate_furl))
    stats_gatherer_furl = attr.ib(default=None, validator=attr.validators.optional(validate_furl))


@attr.s(frozen=True)
class SubscriptionDetails(object):
    bucketname = attr.ib()
    oldsecrets = attr.ib(convert=freeze)
    customer_email = attr.ib()
    customer_pgpinfo = attr.ib()

    product_id = attr.ib()

    # Referencing opaque stripe identifiers
    customer_id = attr.ib()
    subscription_id = attr.ib()

    introducer_port_number = attr.ib()
    storage_port_number = attr.ib()
