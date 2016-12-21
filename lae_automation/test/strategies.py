# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
This module implements various Hypothesis strategies useful for
QuickCheck-style testing of ``lae_automation`` APIs.
"""

from os import devnull
from string import uppercase, lowercase, ascii_letters, digits
from base64 import b32encode

from hypothesis import strategies

from allmydata.util import keyutil

from lae_automation import signup

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
    )

def customer_id():
    return strategies.binary(min_size=10, max_size=10).map(
        lambda b: b'cus_' + b.encode('base64').strip('=\n')
    )

def bucket_name():
    return strategies.tuples(
        subscription_id(),
        customer_id(),
    ).map(
        lambda (s, c): signup.get_bucket_name(s, c)
    )

def ipv4_address():
    return strategies.lists(
        elements=strategies.integers(min_value=0, max_value=255),
        min_size=4, max_size=4,
    ).map(
        lambda parts: "{}.{}.{}.{}".format(*parts)
    )

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


def domain():
    return strategies.lists(
        strategies.text(min_size=1, average_size=8, max_size=255),
        min_size=1, average_size=2
    ).map(
        lambda parts: u".".join(parts)
    ).filter(
        lambda domain: len(domain) <= 255,
    )


def email():
    # Not capable of generating the full range of legal email
    # addresses (RFC 5321 ``Mailbox`` productions).  Might be worth
    # expanding someday.  Or might not.
    return strategies.builds(
        u"{local}@{domain}".format,
        local=_local_part(),
        domain=domain(),
    )

def swissnum():
    return strategies.binary(
        min_size=Tub.NAMEBITS / 8,
        max_size=Tub.NAMEBITS/  8,
    ).map(
        lambda b: b32encode(b).rstrip("=").lower()
    )

def port_number():
    return strategies.integers(
        min_value=0, max_value=2 ** 16 - 1
    )

def furl():
    # XXX Not well factored probably.
    return strategies.builds(
        lambda pem, addr, port, swissnum: encode_furl(
            Tub(pem).getTubID(),
            ["{}:{}".format(addr, port)],
            swissnum,
        ),
        node_pem(),
        ipv4_address(),
        port_number(),
        swissnum(),
    )

def _add_furl(swissnum, target):
    def add(config):
        num = config.pop(swissnum)
        tubID = Tub(config["node_pem"]).getTubID()
        config[target] = encode_furl(tubID, ["127.0.0.1:12345"], num)
        return config
    return add

def node_pem():
    # XXX Slow to generate these.  Variations probably matter little.
    # Still, they might.  Do something better.
    NODE_PEM = """\
-----BEGIN CERTIFICATE-----
MIICjzCCAfgCAQEwDQYJKoZIhvcNAQEEBQAwgY8xEDAOBgNVBAsTB2V4YW1wbGUx
EDAOBgNVBAoTB2V4YW1wbGUxFDASBgNVBAMTC2V4YW1wbGUuY29tMRAwDgYDVQQI
EwdleGFtcGxlMQswCQYDVQQGEwJVUzEiMCAGCSqGSIb3DQEJARYTZXhhbXBsZUBl
eGFtcGxlLmNvbTEQMA4GA1UEBxMHZXhhbXBsZTAeFw0xNDAyMTIwMDMxMzlaFw0x
NTAyMTIwMDMxMzlaMIGPMRAwDgYDVQQLEwdleGFtcGxlMRAwDgYDVQQKEwdleGFt
cGxlMRQwEgYDVQQDEwtleGFtcGxlLmNvbTEQMA4GA1UECBMHZXhhbXBsZTELMAkG
A1UEBhMCVVMxIjAgBgkqhkiG9w0BCQEWE2V4YW1wbGVAZXhhbXBsZS5jb20xEDAO
BgNVBAcTB2V4YW1wbGUwgZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAKkRahIc
Fp0V44QYpO9ue7mjZNbZYPAC8caoQC1jUgL42CT40PcoOiZWLgRk+Qw6P7PoJzO/
T4ufK0qoPUJm1jErDRWy9eWlGLE0grPECM+jxFfLXxJLKdPtuwMA8Ip72JMirFN5
Y/JTBZOR3j5a/mbY5tcRqgffKxm4QQegnhiBAgMBAAEwDQYJKoZIhvcNAQEEBQAD
gYEAWANPpp985nXMoIwHlsSMm8ijkk7XQU3oioCYDcM6pLT+mvBDe1mZc8mUlrWy
Zo/lT6HF44SHIZ0zCgPYwTpWV6C0K+/kKlYBERZ3ajrzGf5ACfUTNyk5P81C68mc
9fQ7lhq1iuNKzVh8b746Z4ufn6iI1VygnyOQ1hZ/lOX56TA=
-----END CERTIFICATE-----
-----BEGIN RSA PRIVATE KEY-----
MIICXQIBAAKBgQCpEWoSHBadFeOEGKTvbnu5o2TW2WDwAvHGqEAtY1IC+Ngk+ND3
KDomVi4EZPkMOj+z6Cczv0+LnytKqD1CZtYxKw0VsvXlpRixNIKzxAjPo8RXy18S
SynT7bsDAPCKe9iTIqxTeWPyUwWTkd4+Wv5m2ObXEaoH3ysZuEEHoJ4YgQIDAQAB
AoGBAIHNSO6WehYolAD7GsZowL0J4YXCZ1ZeLFolGwC93F1DyE66aVUYoWyFhdcB
3uOwZPAvMMnd+6hqj8ZF3KJ6ab8eSq3hQQqm4EKHPN2DHHQPWppjWsFtUjG6P4FJ
tuHu0JD0u10LxdyW240fcptdorAl+e9g6uphCpQ/9h5TgehBAkEA2iLbbp/MM1O8
Foz+JXY1FfNIgh67Di1BuG8asnX7lsPXI/HsNHPevvOxBbJLh0NEB4AW7Lva0pHH
DaE+YPPLPQJBAMZqJ87+6iC8V3pRRDi+pfjZ8cRFBIJtbKMloP02/k9dEHeNepZO
5EcztYPZbgNC6U01ZD5Gyhnc9t5tPfSg5pUCQDASnH9NsifhnULvAZdp7JsQyXr7
oMeoC6LEwYJw4+g+8qvWRfLtUjqM5AdYWrLNjTGF9gdoAvqC6/ZCAchGEhUCQE/A
P3v+DlFWIrsxiwBb8Q5TW9AOBb//B5mT+F+PCS0RNRs4rLtZvnu4Fw+GB6gb7vZv
rXkyru0yWbARrMN1IPkCQQCXNEoKmNSgFOCruE6i7vjkdllkv+KdXQ13l305qlHC
OzEHlEY+wXPu5R9v3tt2Ktfvfm2Mpfr9Gn+6CUXYn/+P
-----END RSA PRIVATE KEY-----
"""
    return strategies.sampled_from([NODE_PEM])

def introducer_configuration():
    return strategies.fixed_dictionaries({
        "root": absolute_path(),
        "port": port_number(),
        "node_pem": node_pem(),

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

def storage_configuration(keypair=keyutil.make_keypair()):
    return strategies.fixed_dictionaries({
        "root": absolute_path(),
        "port": port_number(),
        "node_pem": node_pem(),
        # Uses real randomness.  Should parameterize the prng.
        "node_privkey": strategies.sampled_from([keypair[0]]),

        "bucket_name": bucket_name(),
        "publichost": ipv4_address(),
        "privatehost": ipv4_address(),
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
    # So far as I can tell, based on ``1aws ec2 create-key-pair help``
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

def deployment_configuration():
    return strategies.builds(
        signup.DeploymentConfiguration,
        products=strategies.just([{"foo": "bar"}]),
        s3_access_key_id=aws_access_key_id(),
        s3_secret_key=aws_secret_key(),
        amiimageid=strategies.just("ami-018c9568"),
        instancesize=strategies.just("t1.micro"),
        usertoken=strategies.none(),
        producttoken=strategies.none(),

        ssec2_access_key_id=aws_access_key_id(),
        ssec2_secret_path=absolute_path(),

        ssec2admin_keypair_name=aws_keypair_name(),
        ssec2admin_privkey_path=absolute_path(),

        monitor_pubkey_path=absolute_path(),
        monitor_privkey_path=absolute_path(),

        secretsfile=strategies.just(open(devnull)),
    )


def subscription_details():
    return strategies.builds(
        signup.SubscriptionDetails,
        bucketname=bucket_name(),
        oldsecrets=strategies.none(),
        customer_email=email(),
        customer_pgpinfo=strategies.none(),
        product_id=strategies.just("S4_consumer_iteration_2_beta1_2014-05-27"),
        customer_id=customer_id(),
        subscription_id=subscription_id(),
        introducer_port_number=port_number(),
        storage_port_number=port_number(),
    ).filter(
        lambda details: details.introducer_port_number != details.storage_port_number,
    )
