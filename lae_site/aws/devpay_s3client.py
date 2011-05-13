# Reference:
# http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?WebAWSCalls.html

# *POTENTIAL BUG*: txaws only supports a single value for each header, but
# the legacy API for license service required multiple x-amz-security-token
# values.  The latest API only appears to require one, but this needs
# vetting.

from txaws.s3.client import S3Client, Query


class DevPayS3Client (S3Client):
    """
    This wraps txaws.s3.client.S3Client to make it DevPay aware.
    """
    def __init__(self, creds, devpayusertoken, endpoint=None):
        S3Client.__init__(
            self,
            creds,
            endpoint,
            query_factory = self._make_query_factory(devpayusertoken),
            )

    # Private
    @staticmethod
    def _make_query_factory(devpayusertoken):
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
            amz_headers['security-token'] = devpayusertoken

            kwargs['amz_headers'] = amz_headers

            return Query(**kwargs)

        return make_query
