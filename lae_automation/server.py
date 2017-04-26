# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

from OpenSSL.crypto import FILETYPE_PEM

from twisted.internet.ssl import KeyPair
from twisted.python.filepath import FilePath

from foolscap.api import Tub

from allmydata.util import keyutil

from .model import make_external_furl

CONFIGURE_TAHOE_PATH = FilePath(__file__).sibling(b"configure-tahoe")


# These are legacy junk that can be cleaned up after the
# external_introducer_furl in oldsecrets get removed.  Nothing actually _uses_
# these values anymore.
INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

def secrets_to_legacy_format(secrets):
    def nodeid(pem):
        # XXX < warner> we're moving to non-foolscap ed25519 pubkey
        return Tub(certData=pem).tubID.lower()

    return dict(
        user_token=None,
        product_token=None,

        introducer_nodeid=nodeid(secrets["introducer"]["node_pem"]),
        introducer_node_pem=secrets["introducer"]["node_pem"],

        publichost=secrets["storage"]["publichost"],
        privatehost=secrets["storage"]["privatehost"],

        # This gets rewritten here but it's probably still garbage.  If you
        # really want the external introducer furl, look at
        # SubscriptionDetails.external_introducer_furl instead.
        external_introducer_furl=make_external_furl(
            secrets["storage"]["introducer_furl"],
            secrets["storage"]["publichost"],
            INTRODUCER_PORT,
        ),
        internal_introducer_furl=secrets["storage"]["introducer_furl"],

        bucket_name=secrets["storage"]["bucket_name"],
        server_node_privkey=secrets["storage"]["node_privkey"],
        server_nodeid=nodeid(secrets["storage"]["node_pem"]),
        server_node_pem=secrets["storage"]["node_pem"],

        access_key_id=secrets["storage"]["s3_access_key_id"],
        secret_key=secrets["storage"]["s3_secret_key"],
    )

def marshal_tahoe_configuration(
        introducer_pem,
        storage_pem, storage_privkey,
        introducer_port, storageserver_port,
        bucket_name, publichost, privatehost, introducer_furl,
        s3_access_key_id, s3_secret_key,
        log_gatherer_furl=None,
        stats_gatherer_furl=None,
):
    if log_gatherer_furl is None:
        log_gatherer_furl = ""
    if stats_gatherer_furl is None:
        stats_gatherer_furl = ""
    return dict(
        introducer=dict(
            port=introducer_port,
            node_pem=introducer_pem,
            introducer_furl=introducer_furl,
            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        ),
        storage=dict(
            port=storageserver_port,
            node_pem=storage_pem,
            node_privkey=storage_privkey,
            bucket_name=bucket_name,
            publichost=publichost,
            privatehost=privatehost,
            introducer_furl=introducer_furl,
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        ),
    )


def new_tahoe_configuration(deploy_config, bucketname, publichost, privatehost, introducer_port, storageserver_port):
    """
    Create brand new secrets and configuration for use by an
    introducer/storage pair.
    """
    base_name = dict(
        organizationName=b"Least Authority Enterprises",
        organizationalUnitName=b"S4",
        emailAddress=bucketname,
    )

    keypair = KeyPair.generate(size=2048)
    introducer_certificate = keypair.selfSignedCert(
        serialNumber=1,
        commonName=b"introducer",
        **base_name
    )
    storage_certificate = keypair.selfSignedCert(
        serialNumber=1,
        commonName=b"storage",
        **base_name
    )
    def pem(key, cert):
        return b"\n".join((key.dump(FILETYPE_PEM), cert.dump(FILETYPE_PEM)))

    introducer_tub = Tub(certData=pem(keypair, introducer_certificate))
    introducer_tub.setLocation("{}:{}".format(publichost, introducer_port))
    storage_tub = Tub(certData=pem(keypair, storage_certificate))

    return marshal_tahoe_configuration(
        introducer_pem=introducer_tub.getCertData().strip(),

        storage_pem=storage_tub.getCertData().strip(),
        storage_privkey=keyutil.make_keypair()[0] + b"\n",

        introducer_port=introducer_port,
        storageserver_port=storageserver_port,

        bucket_name=bucketname,
        publichost=publichost,
        privatehost=privatehost,
        # The object of the reference is irrelevant.  The furl will
        # get hooked up to something else when Tahoe really runs.
        # Just need to pass something _weak referenceable_!  Which
        # rules out a lot of things...
        introducer_furl=introducer_tub.registerReference(introducer_tub),

        s3_access_key_id=deploy_config.s3_access_key_id,
        s3_secret_key=deploy_config.s3_secret_key,

        log_gatherer_furl=deploy_config.log_gatherer_furl,
        stats_gatherer_furl=deploy_config.stats_gatherer_furl,
    )
