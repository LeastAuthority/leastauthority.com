
from txaws.route53.model import RRSet
from txaws.route53.client import _Route53Client, RECORD_TYPES

def _safe_get_rrset_RESOURCE(self, label, type, rrset):
    # http://docs.aws.amazon.com/Route53/latest/APIReference/API_ResourceRecord.html
    resourcerecords = rrset.find("./ResourceRecords")
    if resourcerecords is None:
        return None
    records = resourcerecords.iterfind("./ResourceRecord")
    # The docs say TTL is optional but I think that means rrsets that
    # contain something other than ResourceRecord may not have it.
    # Hopefully it's always present for ResourceRecord-tyle
    # ResourceRecordSets?
    ttl = int(rrset.find("TTL").text)
    return RRSet(
        label=label,
        type=type,
        ttl=ttl,
        records={
            RECORD_TYPES[type].basic_from_element(element)
            for element
            in records
            # This is what's changed from upstream.  We'll just drop anything
            # we don't recognize.  That's sufficient for our purposes for now.
            if type in RECORD_TYPES
        },
    )


def patch():
    _Route53Client._get_rrset_RESOURCE = _safe_get_rrset_RESOURCE
