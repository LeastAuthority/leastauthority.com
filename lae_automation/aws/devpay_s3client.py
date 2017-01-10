# Reference:
# http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?WebAWSCalls.html

class DependencyError(Exception):
    pass

REQUIRED_S3_API_VERSION = "2006-03-01"

import txaws
from txaws import version as txaws_version

def compare_to_ascii(a, b):
    # b is always ASCII, a may not be.
    if isinstance(a, unicode):
        b = b.decode('ascii')
    return a == b

from txaws.s3.client import S3Client, Query

class DevPayS3Client(S3Client):
    """
    This wraps txaws.s3.client.S3Client to make it DevPay aware.
    """
    def __init__(self, creds, usertoken, producttoken=None, endpoint=None):
        S3Client.__init__(
            self,
            creds,
            endpoint,
            query_factory = self._make_query_factory(usertoken, producttoken),
            )

    # Private
    @staticmethod
    def _make_query_factory(usertoken, producttoken):
        """
        Given a DevPay UserToken, return a Query factory.
        """

        def make_query(**kwargs):
            """
            NOTE: This relies on the implementation detail that
            S3Client always uses keyword arguments in every call to
            the query_factory.
            """

            amz_headers = kwargs.get('amz_headers', {})

            assert 'security-token' not in amz_headers, `amz_headers`
            if producttoken:
                amz_headers['security-token'] = (usertoken, producttoken)
            else:
                amz_headers['security-token'] = usertoken

            kwargs['amz_headers'] = amz_headers

            return Query(**kwargs)

        return make_query
