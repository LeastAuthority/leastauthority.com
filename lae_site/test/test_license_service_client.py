from twisted.trial.unittest import TestCase

from lae_site.license_service_client import LicenseServiceClient, ActivateHostedProductResponse, ResponseParseError


class LicenseServiceClientTests (TestCase):

    def test__collapse_params(self):
        """
        Test vectors cut'n'pasted from documented examples:

        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
        """
        input = [
            ('Action', 'CreateQueue'),
            ('QueueName', 'queue2'),
            ('AWSAccessKeyId', '0A8BDF2G9KCB3ZNKFA82'),
            ('SignatureVersion', '1'),
            ('Expires', '2007-01-12T12:00:00Z'),
            ('Version', '2006-04-01'),
            ]

        expected = 'ActionCreateQueueAWSAccessKeyId0A8BDF2G9KCB3ZNKFA82Expires2007-01-12T12:00:00ZQueueNamequeue2SignatureVersion1Version2006-04-01'

        actual = LicenseServiceClient._collapse_params ( input )

        self.assertEqual ( expected, actual )

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
