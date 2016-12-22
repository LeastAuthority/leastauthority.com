__all__ = ["get_route53_client"]

from txaws.client.base import BaseClient

_REGISTRATION_ENDPOINT = "route53domains.us-east-1.amazonaws.com"
_OTHER_ENDPOINT = "route53.amazonaws.com"

def get_route53_client(aws):
    return aws.get_client(
        _Route53Client,
        creds=aws.creds,
        registration_endpoint=_REGISTRATION_ENDPOINT,
        other_endpoint=_OTHER_ENDPOINT,
    )

class _Route53Client(BaseClient):
    def list_resource_record_sets(self, zone_id, identifier, maxitems, name, type):
        """
        http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListResourceRecordSets.html
        """
        args = [
            ("identifier", identifier),
            ("maxitems", str(maxitems)),
            ("name", name),
            ("type", type),
        ]

        query = Query(
            action="GET",
            creds=self.creds,
            endpoint=self.endpoint,
            zone_id=zone_id,
            path="/2013-04-01/hostedzone/{zone_id}/rrset",
            args=args,
        )
        d = query.submit()
        d.addCallback(rrset_from_response)
        return d
            
    def change_resource_record_sets(self):
        """
        http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html
        """
