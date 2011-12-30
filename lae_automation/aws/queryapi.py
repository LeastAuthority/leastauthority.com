
import urllib, time, re
from hashlib import sha1
from base64 import b64encode

from xml.parsers.expat import ExpatError
from xml.etree import ElementTree
from txaws.util import XML

from lae_util.http_client import make_http_request
from lae_util.no_overwrite import update_by_keywords_without_overwrite
from lae_util import timestamp
from txaws.ec2.client import EC2Client, Parser as txaws_ec2_Parser
from txaws.service import AWSCredentials, AWSServiceEndpoint


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
    #print >>sys.stderr, text
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


EC2_PUBLIC_DNS = re.compile(r'^ec2(-(0|([1-9][0-9]{0,2}))){4}\.')

class AddressParser(txaws_ec2_Parser):
    def describe_instances(self, xml_bytes):
        addresslist = []
        doc = xml_parse(xml_bytes)
        node = xml_find(doc, u'reservationSet')
        itemlist = node.findall(u'item')
        for item in itemlist:
            iset = xml_find(item, u'instancesSet')
            inneritem = xml_find(iset, u'item')
            try:
                publichost = xml_find(inneritem, u'dnsName').text
                publichost = publichost.strip()
                m = EC2_PUBLIC_DNS.match(publichost)
                if m:
                    # If the name matches EC2_PUBLIC_DNS, we prefer to extract the IP address
                    # to eliminate the DNS point of failure.
                    publichost = publichost[len('ec2-'):].split('.')[0].replace('-', '.')
                addresslist.append(publichost)
            except ResponseParseError:
                return None

            if not publichost:
                return None
        return addresslist

def get_EC2_addresses(ec2accesskeyid, ec2secretkey, endpoint_uri, *instance_ids):
    """
    Reference: http://docs.amazonwebservices.com/AWSEC2/latest/APIReference/index.html?ApiReference-query-DescribeInstances.html
    """
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=ec2creds, endpoint=endpoint, parser=AddressParser())
    return client.describe_instances(*instance_ids)
