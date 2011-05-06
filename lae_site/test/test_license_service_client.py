from twisted.trial.unittest import TestCase
from twisted.internet.defer import Deferred
from twisted.internet import reactor

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint

from lae_site.license_service_client import LicenseServiceClient, ActivateHostedProductResponse, ResponseParseError


class LicenseServiceClientTests (TestCase):

    def setUp(self):

        self._trimmed_fake_params = dict(FAKE_PARAMS)

        # Remove these params which are automatically added in _build_request_uri:
        del self._trimmed_fake_params['AWSAccessKeyId']
        del self._trimmed_fake_params['Expires']
        del self._trimmed_fake_params['SignatureVersion']
        del self._trimmed_fake_params['Version']

        def fake_http_request(uri, method='GET'):
            self.assertEqual ( EXPECTED_BUILT_URL, uri )
            self.assertEqual ( 'GET', method )

            d = Deferred()
            reactor.callLater(0, d.callback, SAMPLE_RESPONSE)
            return d

        self.lsc = LicenseServiceClient(
            creds=AWSCredentials(access_key=FAKE_AWS_ACCESS_KEY_ID, secret_key=FAKE_HMAC_KEY),
            endpoint=AWSServiceEndpoint(uri=FAKE_ENDPOINT_URI),
            make_http_request=fake_http_request,
            get_time=lambda : FAKE_TIME_STAMP,
            )

    def test__send_request(self):

        d = self.lsc._send_request ( **self._trimmed_fake_params )

        def check_response(actual):
            self.assertEqual ( SAMPLE_RESPONSE, actual )

        d.addCallback(check_response)
        return d

    def test__build_request_url(self):

        actual = self.lsc._build_request_url ( self._trimmed_fake_params )

        self.assertEqual ( EXPECTED_BUILT_URL, actual )

    def test__calc_signature(self):

        actual = self.lsc._calc_signature ( FAKE_PARAMS.items() )

        self.assertEqual( EXPECTED_BASE64_SIGNATURE, actual )

    def test__collapse_params(self):
        """
        Test vectors cut'n'pasted from documented examples:

        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
        """
        actual = LicenseServiceClient._collapse_params ( FAKE_PARAMS.items() )

        self.assertEqual ( EXPECTED_COLLAPSED_PARAMS, actual )

    def test__collapse_params_does_not_url_encode(self):
        """
        From the reference: "Do not URL encode the parameter values."
        """
        name = 'foo'
        value = 'I have characters urlencoding escaped characters: +*&?\n\0'
        input = [ (name, value) ]
        expected = name + value
        actual = LicenseServiceClient._collapse_params ( input )

        self.assertEqual ( expected, actual )


class ActivateHostedProductResponseTests (TestCase):

    def test_parse_positive(self):

        try:
            ahpr = ActivateHostedProductResponse.parse ( SAMPLE_RESPONSE )

        except ResponseParseError, e:
            self.fail('Failed to parse valid response: {0}'.format(e))

        else:
            self.assertEqual ( FAKE_USERTOKEN, ahpr.usertoken )
            self.assertEqual ( FAKE_PID, ahpr.pid )


    def test_parse_negative(self):
        for invalid in INVALID_XMLS:
            try:
                t = ActivateHostedProductResponse.parse ( invalid )
            except ResponseParseError:
                continue
            else:
                self.fail('Incorrectly parsed {0!r} into: {1!r}'.format(invalid, t))


# Test vectors cut'n'pasted from documented examples:
# Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
FAKE_USERTOKEN = '{UserToken}AAAHVXNlclRrbgfOpSykBAXO7g/zG....[long encoded token]...'
FAKE_PID = 'PMNGLKRRYHLOXDQKEMKLRTBAULA'
FAKE_REQ_ID = 'cb919c0a-9bce-4afe-9b48-9bdf2412bb67'

SAMPLE_RESPONSE = """
<ActivateHostedProductResponse>
   <ActivateHostedProductResult>
      <UserToken>
         {usertoken}
      </UserToken>
      <PersistentIdentifier>
         {pid}
      </PersistentIdentifier>
   </ActivateHostedProductResult>
   <ResponseMetadata>
      <RequestId>
         {reqid}
      </RequestId>
   </ResponseMetadata>
</ActivateHostedProductResponse>
""".format(
    usertoken=FAKE_USERTOKEN,
    pid=FAKE_PID,
    reqid=FAKE_REQ_ID,
    )

INVALID_XMLS = [
    "$BLORG!  I AM NOT XML SO NEENER NEENER!",
    "<wrongShape1 />",
    """
<ActivateHostedProductResponse>
  Invalid shape 2
</ActivateHostedProductResponse>
""",
    """
<ActivateHostedProductResponse>
   <ActivateHostedProductResult>
      <UserToken>
         {usertoken}
      </UserToken>
   </ActivateHostedProductResult>
   <ResponseMetadata>
      <RequestId>
         {reqid}
      </RequestId>
   </ResponseMetadata>
</ActivateHostedProductResponse>
""".format ( usertoken=FAKE_USERTOKEN, reqid=FAKE_REQ_ID ),
    """
<ActivateHostedProductResponse>
   <ActivateHostedProductResult>
      <PersistentIdentifier>
         {pid}
      </PersistentIdentifier>
   </ActivateHostedProductResult>
   <ResponseMetadata>
      <RequestId>
         {reqid}
      </RequestId>
   </ResponseMetadata>
</ActivateHostedProductResponse>
""".format ( pid=FAKE_PID, reqid=FAKE_REQ_ID ),
    ]

FAKE_AWS_ACCESS_KEY_ID = '0A8BDF2G9KCB3ZNKFA82'
FAKE_TIME_STAMP = '2007-01-12T12:00:00Z'
FAKE_VERSION = '2006-04-01'

FAKE_PARAMS = {
    'Action':'CreateQueue',
    'QueueName':'queue2',
    'AWSAccessKeyId':FAKE_AWS_ACCESS_KEY_ID,
    'SignatureVersion':'1',
    'Expires':FAKE_TIME_STAMP,
    'Version':FAKE_VERSION,
    }

EXPECTED_COLLAPSED_PARAMS = 'ActionCreateQueueAWSAccessKeyId0A8BDF2G9KCB3ZNKFA82Expires2007-01-12T12:00:00ZQueueNamequeue2SignatureVersion1Version2006-04-01'

FAKE_HMAC_KEY = 'fake-secret-key'

EXPECTED_BASE64_SIGNATURE = 'wlv84EOcHQk800Yq6QHgX4AdJfk='

FAKE_ENDPOINT_URI = 'http://fake-site.faketld/fake_path'

EXPECTED_BUILT_URL = FAKE_ENDPOINT_URI + '?Action=CreateQueue&AWSAccessKeyId=0A8BDF2G9KCB3ZNKFA82&Expires=2007-01-12T12%3A00%3A00Z&QueueName=queue2&SignatureVersion=1&Version=2008-04-28&Signature=%2Bg091tUDDhl8KZmkstGb41D9Ui4%3D'

