
from twisted.web.static import Data
from twisted.web.resource import IResource, Resource

from txaws.service import AWSServiceRegion
from txaws.testing.base import TXAWSTestCase

from lae_util.memoryagent import MemoryAgent
from lae_util.uncooperator import Uncooperator

from lae_automation.route53 import (
    NS, SOA, Name, get_route53_client,
    create_rrset, delete_rrset, upsert_rrset,
)


class POSTableData(Data):
    def render_POST(self, request):
        # XXX TODO Record something for a later assertion?
        return Data.render_GET(self, request)


def static_resource(hierarchy):
    root = Resource()
    for k, v in hierarchy.iteritems():
        if IResource.providedBy(v):
            root.putChild(k, v)
        elif isinstance(v, dict):
            root.putChild(k, static_resource(v))
        else:
            raise NotImplementedError(v)
    return root


class sample_list_resource_record_sets_emptyish_result(object):
    name = u"example.invalid."
    soa = SOA(
        mname=Name(u"1.awsdns-1.net."),
        rname=Name(u"awsdns-hostmaster.amazon.com."),
        serial=1,
        refresh=7200,
        retry=900,
        expire=1209600,
        minimum=86400,
    )
    ns1 = NS(
        nameserver=Name(u"ns-1.awsdns-1.net."),
    )
    ns2 = NS(
        nameserver=Name(u"ns-2.awsdns-2.net."),
    )
    xml = u"""\
<?xml version="1.0"?>
<ListResourceRecordSetsResponse xmlns="https://route53.amazonaws.com/doc/2013-04-01/"><ResourceRecordSets><ResourceRecordSet><Name>{name}</Name><Type>NS</Type><TTL>172800</TTL><ResourceRecords><ResourceRecord><Value>{ns1.nameserver}</Value></ResourceRecord><ResourceRecord><Value>{ns2.nameserver}</Value></ResourceRecord></ResourceRecords></ResourceRecordSet><ResourceRecordSet><Name>{name}</Name><Type>SOA</Type><TTL>900</TTL><ResourceRecords><ResourceRecord><Value>{soa.mname} {soa.rname} {soa.serial} {soa.refresh} {soa.retry} {soa.expire} {soa.minimum}</Value></ResourceRecord></ResourceRecords></ResourceRecordSet></ResourceRecordSets><IsTruncated>false</IsTruncated><MaxItems>100</MaxItems></ListResourceRecordSetsResponse>
""".format(name=name, soa=soa, ns1=ns1, ns2=ns2).encode("utf-8")


class sample_change_resource_record_sets_result(object):
    name = u"example.invalid."
    create_type = u"NS"
    create_rrset = [NS(Name(u"ns1.example.invalid.")), NS(Name(u"ns2.example.invalid."))]

    xml = b"""\
<?xml version="1.0" encoding="UTF-8"?>
<ChangeResourceRecordSetsResponse>
   <ChangeInfo>
      <Comment>string</Comment>
      <Id>string</Id>
      <Status>string</Status>
      <SubmittedAt>timestamp</SubmittedAt>
   </ChangeInfo>
</ChangeResourceRecordSetsResponse>
"""


class ListResourceRecordSetsTestCase(TXAWSTestCase):
    """
    Tests for C{list_resource_record_sets}.
    """
    def test_some_records(self):
        zone_id = b"ABCDEF1234"
        agent = MemoryAgent(static_resource({
            b"2013-04-01": {
                b"hostedzone": {
                    zone_id: {
                        b"rrset": Data(
                            sample_list_resource_record_sets_emptyish_result.xml,
                            b"text/xml",
                        )
                    }
                }
            }
        }))
        aws = AWSServiceRegion(access_key="abc", secret_key="def")
        client = get_route53_client(agent, aws)
        rrsets = self.successResultOf(client.list_resource_record_sets(
            zone_id=zone_id,
        ))
        expected = {
            Name(sample_list_resource_record_sets_emptyish_result.name): {
                sample_list_resource_record_sets_emptyish_result.soa,
                sample_list_resource_record_sets_emptyish_result.ns1,
                sample_list_resource_record_sets_emptyish_result.ns2,
            }
        }
        self.assertEquals(rrsets, expected)


class ChangeResourceRecordSetsTestCase(TXAWSTestCase):
    """
    Tests for C{change_resource_record_sets}.
    """
    def test_some_changes(self):
        zone_id = b"ABCDEF1234"
        agent = MemoryAgent(static_resource({
            b"2013-04-01": {
                b"hostedzone": {
                    zone_id: {
                        b"rrset": POSTableData(
                            sample_change_resource_record_sets_result.xml,
                            b"text/xml",
                        ),
                    }
                },
            },
        }))
        aws = AWSServiceRegion(access_key="abc", secret_key="def")
        client = get_route53_client(agent, aws, Uncooperator())
        self.successResultOf(client.change_resource_record_sets(
            zone_id=zone_id,
            changes=[
                create_rrset(
                    sample_change_resource_record_sets_result.name,
                    sample_change_resource_record_sets_result.create_type,
                    sample_change_resource_record_sets_result.create_rrset,
                ),
                # delete_rrset(
                #     sample_change_resource_record_sets_result.name,
                #     sample_change_resource_record_sets_result.delete_type,
                #     sample_change_resource_record_sets_result.delete_rrset,
                # ),
                # upsert_rrset(
                #     sample_change_resource_record_sets_result.name,
                #     sample_change_resource_record_sets_result.upsert_type,
                #     sample_change_resource_record_sets_result.upsert_rrset,
                # ),
            ],
        ))
