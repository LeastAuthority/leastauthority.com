
from collections import namedtuple

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint

from lae_automation.aws.queryapi import QueryAPIMixin, xml_parse, xml_find


PRODUCTION_LICENSE_SERVICE_ENDPOINT = 'https://ls.amazonaws.com/'


class LicenseServiceClient(QueryAPIMixin):
    __slots__ = ['_creds', '_endpoint']

    def __init__(self, creds=None, endpoint=None):
        if endpoint is None:
            endpoint = AWSServiceEndpoint(PRODUCTION_LICENSE_SERVICE_ENDPOINT)

        assert creds is None or isinstance(creds, AWSCredentials), `creds`
        assert isinstance(endpoint, AWSServiceEndpoint), `endpoint`

        self._creds = creds
        self._endpoint = endpoint

    def activate_desktop_product(self, activationkey, producttoken):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?ActivateDesktopProduct.html
        """
        d = self._send_request(
            Action = 'ActivateDesktopProduct',
            ActivationKey = activationkey,
            ProductToken = producttoken,
            )
        d.addCallback(ActivateDesktopProductResponse.parse)
        return d

    def verify_subscription_by_tokens(self, usertoken, producttoken):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/VerifyProductSubscriptionByTokens.html
        """
        d = self._send_request(
            Action = 'VerifyProductSubscriptionByTokens',
            UserToken = usertoken,
            ProductToken = producttoken,
            )

        d.addCallback(VerifyProductSubscriptionByTokensResponse.parse)
        return d


class ActivateDesktopProductResponse (namedtuple('ActivateDesktopProductResponse',
                                                 ['access_key_id', 'secret_key', 'usertoken'])):
    @classmethod
    def parse(cls, body):
        doc = xml_parse(body)
        node = xml_find(doc, u'ActivateDesktopProductResult')
        access_key_id = xml_find(node, u'AWSAccessKeyId').text.strip()
        secret_key = xml_find(node, u'SecretAccessKey').text.strip()
        usertoken = xml_find(node, u'UserToken').text.strip()

        return cls(access_key_id, secret_key, usertoken)


class VerifyProductSubscriptionByTokensResponse:
    @classmethod
    def parse(cls, body):
        doc = xml_parse(body)
        node = xml_find(doc, u'VerifyProductSubscriptionByTokensResult')
        subscribed = xml_find(node, u'Subscribed').text.strip()
        return subscribed == "true"
