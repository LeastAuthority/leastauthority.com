from __future__ import print_function, unicode_literals

__all__ = [
    "Name", "SOA", "NS",
    "get_route53_client",
]

from urllib import urlencode

import attr
from attr import validators

from twisted.web.http import OK
from twisted.web.http_headers import Headers
from twisted.web.client import readBody
from twisted.python.failure import Failure

from txaws.client.base import BaseClient, BaseQuery
from txaws.service import AWSServiceEndpoint
from txaws.util import XML

_REGISTRATION_ENDPOINT = "https://route53domains.us-east-1.amazonaws.com/"
_OTHER_ENDPOINT = "https://route53.amazonaws.com/"

def get_route53_client(agent, aws):
    """
    Get a non-registration Route53 client.
    """
    return aws.get_client(
        _Route53Client,
        agent=agent,
        creds=aws.creds,
        endpoint=AWSServiceEndpoint(_OTHER_ENDPOINT),
    )


@attr.s(frozen=True)
class Name(object):
    text = attr.ib(validator=validators.instance_of(unicode))

    def __str__(self):
        return self.text.encode("idna")


@attr.s(frozen=True)
class NS(object):
    nameserver = attr.ib(validator=validators.instance_of(Name))

    @classmethod
    def from_element(cls, e):
        return cls(Name(et_is_dumb(e.find("Value").text)))


def et_is_dumb(bytes_or_text):
    if isinstance(bytes_or_text, bytes):
        return bytes_or_text.decode("utf-8")
    return bytes_or_text


@attr.s(frozen=True)
class SOA(object):
    mname = attr.ib(validator=validators.instance_of(Name))
    rname = attr.ib(validator=validators.instance_of(Name))
    serial = attr.ib(validator=validators.instance_of(int))
    refresh = attr.ib(validator=validators.instance_of(int))
    retry = attr.ib(validator=validators.instance_of(int))
    expire = attr.ib(validator=validators.instance_of(int))
    minimum = attr.ib(validator=validators.instance_of(int))

    @classmethod
    def from_element(cls, e):
        mname, rname, serial, refresh, retry, expire, minimum = et_is_dumb(e.find("Value").text).split()
        return cls(
            mname=Name(mname),
            rname=Name(rname),
            serial=int(serial),
            refresh=int(refresh),
            retry=int(retry),
            expire=int(expire),
            minimum=int(minimum),
        )


RECORD_TYPES = {
    u"SOA": SOA,
    u"NS": NS,
}

class _Route53Client(object):
    def __init__(self, agent, creds, endpoint):
        self.agent = agent
        self.creds = creds
        self.endpoint = endpoint

    def change_resource_record_sets(self, zone_id, changes):
        """
        http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html
        """
        query = _ChangeRRSets(
            action="POST",
            creds=self.creds,
            endpoint=self.endpoint,
            zone_id=zone_id,
            changes=changes,
            args=(),
        )
        return query.submit(self.agent)


    def list_resource_record_sets(self, zone_id, identifier=None, maxitems=None, name=None, type=None):
        """
        http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResourceRecordSets.html
        """
        args = []
        if identifier:
            args.append(("identifier", identifier))
        if maxitems:
            args.append(("maxitems", str(maxitems)))
        if name:
            args.append(("name", name))
        if type:
            args.append(("type", type))

        query = _ListRRSets(
            action="GET",
            creds=self.creds,
            endpoint=self.endpoint,
            zone_id=zone_id,
            args=args,
        )
        return query.submit(self.agent)


def require_status(status_codes):
    def check_status_code(response):
        if response.code not in status_codes:
            raise Exception(
                "Unexpected status code: {} (expected {})".format(
                    response.code, status_codes
                )
            )
        return response
    return check_status_code


def annotate_request_uri(uri):
    def annotate(reason):
        return Failure(
            Exception("while requesting", uri, reason.value),
            Exception,
            reason.tb,
        )
    return annotate


@attr.s(frozen=True)
class _Query(object):
    ok_status = (OK,)

    action = attr.ib()
    creds = attr.ib()
    endpoint = attr.ib()
    zone_id = attr.ib()
    args = attr.ib()

    def path(self):
        raise NotImplementedError()

    def body(self):
        return None

    def submit(self, agent):
        base_uri = self.endpoint.get_uri()
        uri = base_uri + self.path() + b"?" + urlencode(self.args)
        d = agent.request(b"GET", uri, Headers(), self.body())
        d.addCallback(require_status(self.ok_status))
        d.addCallback(self.parse)
        d.addErrback(annotate_request_uri(uri))
        return d

    def parse(self, response):
        d = readBody(response)
        d.addCallback(XML)
        d.addCallback(self._extract_result)
        return d


class _RRSets(_Query):
    def path(self):
        return u"2013-04-01/hostedzone/{zone_id}/rrset".format(
            zone_id=self.zone_id
        ).encode("ascii")


@attr.s(frozen=True)
class _ChangeRRSets(_RRSets):
    changes = attr.ib()

    def body(self):
        return FileBodyProducer(
            BytesIO(self._xml_request_body().encode("utf-8"))
        )

    def _xml_request_body(self):
        1/0 # TODO


class _ListRRSets(_RRSets):
    def _extract_result(self, document):
        result = {}
        rrsets = document.iterfind("./ResourceRecordSets/ResourceRecordSet")
        for rrset in rrsets:
            name = Name(et_is_dumb(rrset.find("Name").text))
            type = rrset.find("Type").text
            records = rrset.iterfind("./ResourceRecords/ResourceRecord")
            result.setdefault(name, set()).update({
                RECORD_TYPES[type].from_element(element)
                for element
                in records
            })
        return result


def upsert_rrset(name, type, rrset):
    pass

def create_rrset(name, type, rrset):
    return 3


def create_alias_rrset(name, type, alias):
    pass


def create_failover_rrset(name, type, failover):
    pass


def create_geolocation_rrset(name, type, geolocation):
    pass


def create_latency_based_rrset(name, type, latency):
    pass


def delete_rrset(name, type, rrset):
    pass
