import urllib
from collections import namedtuple
from xml.parsers.expat import ExpatError

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint
from txaws.util import XML

from lae_site.http_client import make_http_request
from lae_site import util


class LicenseServiceClient (object):

    __slots__ = ['_creds', '_endpoint', '_make_http_request']

    def __init__(self, creds, endpoint, make_http_request=make_http_request):

        assert isinstance(creds, AWSCredentials), `creds`
        assert isinstance(endpoint, AWSServiceEndpoint), `creds`

        self._creds = creds
        self._endpoint = endpoint
        self._make_http_request = make_http_request

    def activate_hosted_product(self, activationKey, productToken):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?ActivateHostedProduct.html
        """
        d = self._send_request(
            Action = 'ActivateHostedProduct',
            ActivationKey = activationKey,
            ProductToken = productToken,
            )

        d.addCallback ( ActivateHostedProductResponse.parse )

        return d

    # Private
    def _send_request(self, **params):
        return self._make_http_request(
            url=self._build_request_url(params),
            method='POST')
        
    
    def _build_request_url(self, params):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
        """

        util.update_by_keywords_without_overwrite(
            params,
            AWSAccessKey = self._creds.access_key,
            SignatureVersion = 1,
            Timestamp = util.now(),
            Version = '2008-04-28',
            )

        items = params.items()
        signature = self._calc_signature( items )

        items.append( ('Signature', signature) )

        quote = lambda x: urllib.quote(str(x))
        querystr = '&'.join( '%s=%s' % (k, quote(v)) for (k, v) in items )

        return '%s?%s' % (self._endpoint.get_uri(), querystr)

    def _calc_signature(self, items):
        return self._creds.sign(self._collapse_params(items), hash_type='sha1')

    @staticmethod
    def _collapse_params(items):
        # Sort case-insensitive on the parameter key:
        items.sort( cmp = lambda (a,_x), (b,_y): cmp(a.upper(), b.upper()) )

        return ''.join( (k+v) for (k, v) in items )


class ResponseParseError (Exception):
    pass


class ActivateHostedProductResponse (namedtuple('ActivateHostedProductResponse', ['usertoken', 'pid'])):

    @classmethod
    def parse(cls, body):
        try:
            doc = XML(body)
        except ExpatError, e:
            raise ResponseParseError(e)

        node = _xml_find(doc, u'ActivateHostedProductResult')
        usertoken = _xml_find(node, u'UserToken').text.strip()
        pid = _xml_find(node, u'PersistentIdentifier').text.strip()

        return cls(usertoken, pid)


def _xml_find(node, key):
    r = node.find(key)
    if r is None:
        raise ResponseParseError('Node not found: {0}'.format(key))
    else:
        return r
