
import urllib, time
from hashlib import sha1
from base64 import b64encode

from xml.parsers.expat import ExpatError
from xml.etree import ElementTree
from txaws.util import XML

from lae_util.http_client import make_http_request
from lae_util.no_overwrite import update_by_keywords_without_overwrite
from lae_util import timestamp


def _xor(a, b):
    return "".join([chr(ord(c) ^ ord(b)) for c in a])

def hmac_sha1(tag, data):
    key = (tag + "\x00"*64)[:64]
    ikey = _xor(key, "\x36")
    okey = _xor(key, "\x5c")
    h1 = sha1(ikey + data).digest()
    h2 = sha1(okey + h1).digest()
    return h2


class QueryAPIMixin:
    def _send_request(self, **params):
        url = self._build_request_url(params)
        return make_http_request(url)

    def _build_request_url(self, params):
        """
        Reference: http://docs.amazonwebservices.com/AmazonDevPay/latest/DevPayDeveloperGuide/index.html?LSAPI_Auth_REST.html
        """

        # Make a copy because we will modify the result:
        params = dict(params)

        if self._creds is not None:
            update_by_keywords_without_overwrite(
                params,
                AWSAccessKeyId = self._creds.access_key,
                SignatureVersion = '1',
                Expires = timestamp.format_iso_time(time.time() + 15*60),
                )
        update_by_keywords_without_overwrite(
            params,
            Version = '2008-04-28',
            )

        items = params.items()

        if self._creds is not None:
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


class ResponseParseError(Exception):
    pass


if hasattr(ElementTree, 'ParseError'):
    xml_exceptions = (ExpatError, ElementTree.ParseError)
else:
    xml_exceptions = (ExpatError)


def xml_parse(text):
    print text
    try:
        return XML(text)
    except xml_exceptions, e:
        raise ResponseParseError(e)


def xml_find(node, key):
    r = node.find(key)
    if r is None:
        raise ResponseParseError('Node not found: %r' % (key,))
    else:
        return r
