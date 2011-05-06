from twisted.trial.unittest import TestCase

from lae_site.license_service_client import ActivateHostedProductResponse, ResponseParseError


class ActivateHostedProductResponseTests (TestCase):

    def test_parse_positive(self):

        try:
            ahpr = ActivateHostedProductResponse.parse ( SAMPLE_RESPONSE )

        except ResponseParseError, e:
            self.fail('Failed to parse valid response: {0}'.format(e))

        else:
            self.assertEqual ( FAKE_USERTOKEN, ahpr.usertoken )
            self.assertEqual ( FAKE_PID, ahpr.pid )



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
