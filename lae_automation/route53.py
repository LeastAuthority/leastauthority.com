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
    def __init__(self, **kw):
        pass

    def destroy(self, *a, **kw):
        pass

    def create(self, *a, **kw):
        pass

    
