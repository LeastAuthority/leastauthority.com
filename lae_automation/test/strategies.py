# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module implements various Hypothesis strategies useful for
QuickCheck-style testing of ``lae_automation`` APIs.
"""

from os import devnull
from string import uppercase, lowercase, ascii_letters, ascii_lowercase, digits
from base64 import b32encode
from itertools import count, islice

from hypothesis import strategies

from twisted.python.url import URL

from allmydata.util import keyutil

from lae_automation import model, signup, server

from foolscap.api import Tub
from foolscap.furl import encode_furl

_BASE32_CHARS = lowercase + "234567"

def nickname():
    return strategies.text(
        alphabet=_BASE32_CHARS, min_size=24, max_size=24
    )

def subscription_id():
    return strategies.binary(min_size=10, max_size=10).map(
        lambda b: b'sub_' + b.encode('base64').strip('=\n')
    ).map(
        lambda s: s.decode("ascii")
    )

def customer_id():
    return strategies.binary(min_size=10, max_size=10).map(
        lambda b: b'cus_' + b.encode('base64').strip('=\n')
    ).map(
        lambda s: s.decode("ascii")
    )

def bucket_name():
    return strategies.tuples(
        subscription_id(),
        customer_id(),
    ).map(
        lambda (s, c): signup.get_bucket_name(s, c)
    )

def ipv4_addresses():
    return strategies.lists(
        elements=strategies.integers(min_value=0, max_value=255),
        min_size=4, max_size=4,
    ).map(
        lambda parts: u"{}.{}.{}.{}".format(*parts)
    )
ipv4_address = ipv4_addresses

def aws_access_key_id():
    return strategies.text(
        alphabet=uppercase + digits,
        min_size=20,
        max_size=20,
    )

def aws_secret_key():
    # Maybe it's base64 encoded?
    return strategies.text(
        alphabet=ascii_letters + digits + "/+",
        min_size=40,
        max_size=40,
    )

def relative_path():
    return strategies.characters(
        blacklist_characters=[u"\0", u"/"]
    )

def absolute_path():
    return relative_path().map(
        lambda p: u"/" + p
    )

def _local_part():
    alphabet = "".join((
        digits,
        ascii_letters,
        "!#$%&'*+-/=?^_`{|}~",
    )).decode("ascii")
    return strategies.text(
        alphabet=alphabet,
        min_size=1,
        average_size=8,
        max_size=64,
    )


def idna_text(**kw):
    # It's really expensive to build up a comprehensive list of code points
    # that are acceptable in an (I)nternational (D)omain (N)ame.  Also, it may
    # not actually be possible to decide if a code point is usable in
    # isolation - context may matter (I'm not sure).  So just build up a bunch
    # of safe stuff including _some_ non-ASCII and hopefully that will be good
    # enough.  http://www.unicode.org/faq/idn.html#33 says some stuff for
    # someone intrepid. -exarkun
    idna_alphabet = (
        ascii_lowercase.decode("ascii") +
        digits.decode("ascii") + u"".join([
            # Here's a random assortment of code points from different
            # categories which should all be safe to include in an IDN.  This
            # is far from comprehensive but it avoids the massive expense of
            # building the exactly correct alphabet.
            u'2', u'_', u'\xa0', u'\xa5', u'\xab', u'\xb8', u'\xbb', u'\xe6',
            u'\u0187', u'\u01c8', u'\u0214', u'\u02c1', u'\u02cb', u'\u02d5',
            u'\u02f0', u'\u02f3', u'\u02f8', u'\u035b', u'\u0385', u'\u03a8',
            u'\u0488', u'\u0489', u'\u04d3', u'\u0516', u'\u058a', u'\u05b8',
            u'\u0600', u'\u0601', u'\u0602', u'\u0603', u'\u066a', u'\u06de',
            u'\u094b', u'\u09f3', u'\u09f6', u'\u09fb', u'\u0b57', u'\u0b6b',
            u'\u0bc7', u'\u0c44', u'\u0cca', u'\u0cd6', u'\u0d46', u'\u0ddf',
            u'\u0e50', u'\u0e57', u'\u0f3b', u'\u0f3c', u'\u0fd0', u'\u0fd4',
            u'\u106c', u'\u109c', u'\u1400', u'\u1566', u'\u169c', u'\u16ed',
            u'\u17b4', u'\u17b5', u'\u17db', u'\u19de', u'\u1c3c', u'\u1d1d',
            u'\u1d39', u'\u1d41', u'\u1d5c', u'\u1dab', u'\u1db7', u'\u1e19',
            u'\u1e2d', u'\u1e35', u'\u1f52', u'\u1f8e', u'\u1f8f', u'\u1f98',
            u'\u1f9a', u'\u1f9c', u'\u1fa9', u'\u1fac', u'\u1fbc', u'\u1fcc',
            u'\u1fcd', u'\u1fed', u'\u2003', u'\u2004', u'\u2005', u'\u2007',
            u'\u2009', u'\u200a', u'\u2010', u'\u2012', u'\u2014', u'\u2019',
            u'\u201b', u'\u201c', u'\u201d', u'\u201e', u'\u201f', u'\u202f',
            u'\u2039', u'\u203a', u'\u203f', u'\u2040', u'\u2045', u'\u2054',
            u'\u205d', u'\u205f', u'\u2064', u'\u2081', u'\u20a3', u'\u20ad',
            u'\u20b8', u'\u20dd', u'\u20df', u'\u20e0', u'\u20e2', u'\u20e3',
            u'\u20e4', u'\u2266', u'\u23c9', u'\u247a', u'\u2480', u'\u2495',
            u'\u25eb', u'\u276f', u'\u2773', u'\u2774', u'\u2775', u'\u27ed',
            u'\u27ee', u'\u2850', u'\u288d', u'\u294a', u'\u29d9', u'\u2a0d',
            u'\u2a12', u'\u2a28', u'\u2a46', u'\u2ad0', u'\u2af9', u'\u2b42',
            u'\u2b49', u'\u2c18', u'\u2de0', u'\u2de3', u'\u2e02', u'\u2e03',
            u'\u2e05', u'\u2e09', u'\u2e0a', u'\u2e0c', u'\u2e0d', u'\u2e17',
            u'\u2e1b', u'\u2e1c', u'\u2e1d', u'\u2e20', u'\u2e21', u'\u3015',
            u'\u3016', u'\u3030', u'\u3031', u'\u3039', u'\u309a', u'\u31cf',
            u'\u31db', u'\u4f53', u'\ua082', u'\ua4fd', u'\ua623', u'\ua626',
            u'\ua672', u'\ua6e6', u'\ua700', u'\ua715', u'\ua717', u'\ua874',
            u'\ua904', u'\ua909', u'\uaa54', u'\uad9f', u'\ufd3e', u'\ufe32',
            u'\ufe33', u'\ufe34', u'\ufe37', u'\ufe38', u'\ufe3b', u'\ufe4d',
            u'\ufe4e', u'\ufe4f', u'\ufe56', u'\ufe58', u'\ufe69', u'\uff08',
            u'\uff0d', u'\uff3d', u'\uff3f', u'\uffe0', u'\uffe5',
            u'\U00010140', u'\U00010148', u'\U0001018a', u'\U0001085e',
            u'\U00010a46', u'\U00010e7e', u'\U00011081', u'\U000110bd',
            u'\U00012400', u'\U00012403', u'\U00012412', u'\U00012438',
            u'\U0001243f', u'\U00012453', u'\U0001d049', u'\U0001d134',
            u'\U0001d186', u'\U0001d244', u'\U0001d40c', u'\U0001d4c3',
            u'\U0001d5a9', u'\U0001d66e', u'\U0001d686', u'\U0001d6e8',
            u'\U0001d702', u'\U0001d728', u'\U0001d7dc', u'\U0001f03e',
            u'\U0001f087', u'\U0001f104', u'\U000236ca', u'\U00028ebf',
            u'\U0002937a', u'\U00029944', u'\U0002a39f', u'\U0002b0d3',
            u'\U000e01a9', u'\U000e01ce',
        ])
    )

    return strategies.text(
        alphabet=idna_alphabet,
        **kw
    )


def domains():
    def build_domain(complete, random):
        while complete:
            piece = complete[:random.randrange(64)]
            complete = complete[len(piece):]
            yield piece

    return strategies.builds(
        build_domain,
        complete=idna_text(min_size=1, max_size=255),
        random=strategies.randoms(),
    ).map(
        lambda parts: u".".join(parts)
    )

domain = domains

def emails():
    # Not capable of generating the full range of legal email
    # addresses (RFC 5321 ``Mailbox`` productions).  Might be worth
    # expanding someday.  Or might not.
    return strategies.builds(
        u"{local}@{domain}".format,
        local=_local_part(),
        domain=domains(),
    )

email = emails

def swissnum():
    return strategies.binary(
        min_size=Tub.NAMEBITS / 8,
        max_size=Tub.NAMEBITS/  8,
    ).map(
        lambda b: b32encode(b).rstrip("=").lower()
    )

def port_numbers():
    return strategies.integers(
        min_value=1, max_value=2 ** 16 - 1
    )

port_number = port_numbers

def furls():
    # XXX Not well factored probably.
    return strategies.builds(
        lambda pem, addr, port, swissnum: encode_furl(
            Tub(pem).getTubID(),
            ["{}:{}".format(addr, port)],
            swissnum,
        ),
        node_pems(),
        ipv4_addresses(),
        port_number(),
        swissnum(),
    )
furl = furls

def _add_furl(swissnum, target):
    def add(config):
        num = config.pop(swissnum)
        tubID = Tub(config["node_pem"]).getTubID()
        config[target] = encode_furl(tubID, ["127.0.0.1:12345"], num)
        return config
    return add

def _pregenerate_node_pems():
    # Generating keys and certs is slow.  Do some work up-front so we don't
    # have to do it later.  This also allows some pem re-use which saves us
    # some time.

    from twisted.internet.ssl import KeyPair, PrivateCertificate

    def pems():
        for i in count():
            key = KeyPair.generate()
            cert = key.selfSignedCert(i, commonName=u"lae_automation testing")
            pem = PrivateCertificate.fromCertificateAndKeyPair(cert, key).dumpPEM()
            yield pem.decode("ascii")

    return strategies.sampled_from(list(islice(pems(), 100)))
_node_pems = _pregenerate_node_pems()

def node_pems():
    return _node_pems

node_pem = node_pems


def fake_node_pems():
    # These are totally bogus but they're quick to generate.
    return strategies.builds(
        lambda cert, key: u"".join([
            u"-----BEGIN CERTIFICATE-----\n",
            cert.encode("base64"),
            u"-----END CERTIFICATE-----\n",
            u"-----BEGIN RSA PRIVATE KEY-----\n",
            key.encode("base64"),
            u"-----END RSA PRIVATE KEY-----\n",
        ]),
        cert=strategies.binary(min_size=256, max_size=256),
        key=strategies.binary(min_size=256, max_size=256),
    )



def introducer_configuration(node_pems=node_pems):
    return strategies.fixed_dictionaries({
        "port": port_number(),
        "node_pem": node_pems(),

        "introducer-swissnum": swissnum(),
        "log-gatherer-swissnum": swissnum(),
        "stats-gatherer-swissnum": swissnum(),
    }).map(
        _add_furl("introducer-swissnum", "introducer_furl")
    ).map(
        _add_furl("log-gatherer-swissnum", "log_gatherer_furl")
    ).map(
        _add_furl("stats-gatherer-swissnum", "stats_gatherer_furl")
    )

def storage_configuration(keypair=keyutil.make_keypair(), node_pems=node_pems):
    return strategies.fixed_dictionaries({
        "port": port_number(),
        "node_pem": node_pems(),
        # Uses real randomness.  Should parameterize the prng.
        "node_privkey": strategies.sampled_from([keypair[0]]),

        "bucket_name": bucket_name(),
        "publichost": ipv4_addresses(),
        "privatehost": ipv4_addresses(),
        "s3_access_key_id": aws_access_key_id(),
        "s3_secret_key": aws_secret_key(),

        "introducer-swissnum": swissnum(),
        "log-gatherer-swissnum": swissnum(),
        "stats-gatherer-swissnum": swissnum(),
    }).map(
        _add_furl("introducer-swissnum", "introducer_furl")
    ).map(
        _add_furl("log-gatherer-swissnum", "log_gatherer_furl")
    ).map(
        _add_furl("stats-gatherer-swissnum", "stats_gatherer_furl")
    )

def aws_keypair_name():
    # So far as I can tell, based on ``aws ec2 create-key-pair help``
    return strategies.lists(
        strategies.characters(
            min_codepoint=0,
            max_codepoint=255,
        ),
        min_size=1,
        average_size=8,
        max_size=255,
    ).map(
        u"".join
    )



def kubernetes_namespaces():
    return strategies.sampled_from([u"default", u"staging"])



def urls():
    return strategies.builds(
        URL,
        scheme=strategies.sampled_from([u"http", u"https"]),
        host=domains(),
    )


def deployment_configurations():
    return strategies.builds(
        model.DeploymentConfiguration,
        domain=domains(),
        kubernetes_namespace=kubernetes_namespaces(),
        subscription_manager_endpoint=urls(),

        s3_access_key_id=aws_access_key_id(),
        s3_secret_key=aws_secret_key(),

        introducer_image=strategies.just(u"tahoe-introducer:latest"),
        storageserver_image=strategies.just(u"tahoe-storageserver:latest"),

        ssec2admin_keypair_name=aws_keypair_name(),
        ssec2admin_privkey_path=absolute_path(),

        monitor_pubkey_path=absolute_path(),
        monitor_privkey_path=absolute_path(),

        secretsfile=strategies.just(open(devnull)),
    )

deployment_configuration = deployment_configurations


def old_secrets():
    def same_introducer_furl(config):
        config["storage"]["introducer_furl"] = config["introducer"]["introducer_furl"]
        return config

    return strategies.fixed_dictionaries({
        "introducer": introducer_configuration(),
        "storage": storage_configuration(),
    }).map(
        # Each strategy being composed above wants to generate a data
        # structure with an introducer furl in it.  Real configurations must
        # have the same value in both data structures, though.  So, do it.
        #
        # This reflects the crumminess of our configuration data structures.
        # Hopefully we can improve those (eg, to reduce redundancy) and then
        # improve this strategy.
        same_introducer_furl,
    ).map(
        server.secrets_to_legacy_format
    ).filter(
        lambda secrets: secrets[u"introducer_node_pem"] != secrets[u"server_node_pem"]
    )


def two_distinct_ports():
    def make_ports(port, adjustment):
        other_port = port + adjustment
        if other_port > 65535:
            other_port %= 65536
        if other_port == 0:
            other_port += 1
        return port, other_port

    return strategies.builds(
        make_ports,
        port=port_numbers(),
        adjustment=strategies.integers(min_value=1, max_value=65534),
    )


def subscription_details():
    return strategies.builds(
        lambda ports, **kw: model.SubscriptionDetails(
            introducer_port_number=ports[0],
            storage_port_number=ports[1],
            **kw
        ),
        bucketname=bucket_name(),
        oldsecrets=old_secrets(),
        customer_email=emails(),
        customer_pgpinfo=strategies.none(),
        product_id=strategies.just(u"S4_consumer_iteration_2_beta1_2014-05-27"),
        customer_id=customer_id(),
        subscription_id=subscription_id(),
        ports=two_distinct_ports(),
    )
