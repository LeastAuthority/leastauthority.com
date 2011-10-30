
import urllib, time
from hashlib import sha1
from base64 import b64encode
from collections import namedtuple
from xml.parsers.expat import ExpatError
from xml.etree import ElementTree

from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint
from txaws.util import XML

from lae_site.util.http_client import make_http_request
from lae_site.util.no_overwrite import update_by_keywords_without_overwrite
from lae_site.util import timestamp

PRODUCTION_LICENSE_SERVICE_ENDPOINT = 'https://ls.amazonaws.com/'


def _xor(a, b):
    return "".join([chr(ord(c) ^ ord(b)) for c in a])

def hmac_sha1(tag, data):
    key = (tag + "\x00"*64)[:64]
    ikey = _xor(key, "\x36")
    okey = _xor(key, "\x5c")
    h1 = sha1(ikey + data).digest()
    h2 = sha1(okey + h1).digest()
    return h2


class LicenseServiceClient (object):

    __slots__ = ['_creds', '_endpoint']

    def __init__(self, creds, endpoint=None):

        if endpoint is None:
            endpoint = AWSServiceEndpoint(PRODUCTION_LICENSE_SERVICE_ENDPOINT)

        assert isinstance(creds, AWSCredentials), `creds`
        assert isinstance(endpoint, AWSServiceEndpoint), `creds`

        self._creds = creds
        self._endpoint = endpoint

    def activate_hosted_product(self, activationkey, producttoken):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?ActivateHostedProduct.html
        """
        d = self._send_request(
            Action = 'ActivateHostedProduct',
            ActivationKey = activationkey,
            ProductToken = producttoken,
            )

        d.addCallback ( ActivateHostedProductResponse.parse )

        return d

    # Private
    def _send_request(self, **params):
        url = self._build_request_url(params)
        #print url
        return make_http_request(url)

    def _build_request_url(self, params):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
        """

        # Make a copy because we will modify the result:
        params = dict(params)

        update_by_keywords_without_overwrite(
            params,
            AWSAccessKeyId = self._creds.access_key,
            SignatureVersion = '1',
            Expires = timestamp.format_iso_time(time.time() + 15*60),
            Version = '2008-04-28',
            )

        items = params.items()
        signature = self._calc_signature( items )

        items.append( ('Signature', signature) )

        querystr = '&'.join( ['%s=%s' % (k, urllib.quote(v)) for (k, v) in items] )

        return '%s?%s' % (self._endpoint.get_uri(), querystr)

    def _calc_signature(self, items):
        collapsed = self._collapse_params(items)
        #print collapsed
        return b64encode(hmac_sha1(self._creds.secret_key, collapsed))

    @staticmethod
    def _collapse_params(items):
        # Sort case-insensitive on the parameter key:
        items.sort( cmp = lambda (a,_x), (b,_y): cmp(a.upper(), b.upper()) )

        return ''.join( [(k+v) for (k, v) in items] )


class ResponseParseError (Exception):
    pass


class ActivateHostedProductResponse (namedtuple('ActivateHostedProductResponse', ['usertoken', 'pid'])):

    @classmethod
    def parse(cls, body):
        #print "response body:"
        #print body

        if hasattr(ElementTree, 'ParseError'):
            exceptions = (ExpatError, ElementTree.ParseError)
        else:
            exceptions = (ExpatError)
        try:
            doc = XML(body)
        except exceptions, e:
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
