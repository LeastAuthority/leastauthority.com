from __future__ import print_function, unicode_literals

__all__ = ["get_route53_client"]

from urllib import urlencode

from twisted.web.http import OK
from twisted.web.http_headers import Headers
from twisted.web.client import Agent, readBody

from txaws.client.base import BaseClient, BaseQuery
from txaws.service import AWSServiceEndpoint

_REGISTRATION_ENDPOINT = "https://route53domains.us-east-1.amazonaws.com/"
_OTHER_ENDPOINT = "https://route53.amazonaws.com/"

def get_route53_client(reactor, aws):
    return aws.get_client(
        _Route53Client,
        agent=Agent(reactor),
        creds=aws.creds,
        registration_endpoint=AWSServiceEndpoint(_REGISTRATION_ENDPOINT, ),
        other_endpoint=AWSServiceEndpoint(_OTHER_ENDPOINT),
    )

class _Route53Client(object):
    def __init__(self, agent, creds, registration_endpoint, other_endpoint):
        self.agent = agent
        self.creds = creds
        self.registration_endpoint = registration_endpoint
        self.other_endpoint = other_endpoint

    def list_resource_record_sets(self, zone_id, identifier, maxitems, name, type):
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
            endpoint=self.other_endpoint,
            zone_id=zone_id,
            args=args,
        )
        return query.submit(self.agent)
            
    def change_resource_record_sets(self):
        """
        http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html
        """


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
        raise Exception("while requesting", uri, reason.value)
    return annotate

        
class _Query(object):
    ok_status = (OK,)

    def __init__(self, action, creds, endpoint, zone_id, args):
        self.action = action
        self.creds = creds
        self.endpoint = endpoint
        self.zone_id = zone_id
        self.args = args

    def submit(self, agent,):
        base_uri = self.endpoint.get_uri()
        uri = base_uri + self.path + b"?" + urlencode(self.args)
        d = agent.request(b"GET", uri, Headers())
        d.addCallback(require_status(self.ok_status))
        d.addCallback(self.parse)
        d.addErrback(annotate_request_uri(uri))
        return d

class _ListRRSets(_Query):
    @property
    def path(self):
        return u"2013-04-01/hostedzone/{zone_id}/rrset".format(
            zone_id=self.zone_id
        ).encode("ascii")

    def parse(self, response):
        d = readBody(response)
        d.addCallback(print)
        return d
