__all__ = ["get_route53_client"]

from txaws.client.base import BaseClient, BaseQuery, error_wrapper

_REGISTRATION_ENDPOINT = "route53domains.us-east-1.amazonaws.com"
_OTHER_ENDPOINT = "route53.amazonaws.com"

def get_route53_client(aws):
    return aws.get_client(
        Route53Client,
        creds=self.creds,
        registration_endpoint=_REGISTRATION_ENDPOINT,
        other_endpoint=_OTHER_ENDPOINT,
    )

class _Route53Client(BaseClient):
    pass
