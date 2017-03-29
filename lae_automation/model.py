import attr
from attr import validators

from pem import parse

from twisted.python.url import URL

from foolscap.pb import Tub
from foolscap.furl import decode_furl, encode_furl

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

    products = attr.ib(validator=_validate_products)
    s3_access_key_id = attr.ib(validator=attr.validators.instance_of(unicode))
    s3_secret_key = attr.ib(repr=False, validator=attr.validators.instance_of(unicode))

    # Docker images to run per-subscription containers.
    introducer_image = attr.ib(validator=attr.validators.instance_of(unicode))
    storageserver_image = attr.ib(validator=attr.validators.instance_of(unicode))

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


class NullDeploymentConfiguration(object):
    domain = None
    private_host = None
    kubernetes_namespace = None
    subscription_manager_endpoint = None
    products = ()
    s3_access_key_id = None
    s3_secret_key = None
    introducer_image = None
    storageserver_image = None

    ssec2_access_key_id = None
    ssec2_secret_path = None

    ssec2admin_keypair_name = None
    ssec2admin_privkey_path = None

    monitor_pubkey_path = None
    monitor_privkey_path = None

    secretsfile = open("/dev/full", "a")
    serverinfopath = "/dev/full"

    log_gatherer_furl = None
    stats_gatherer_furl = None



def _parse(pem_str):
    return tuple(sorted(parse(pem_str)))



def _convert_oldsecrets(oldsecrets):
    if oldsecrets:
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
