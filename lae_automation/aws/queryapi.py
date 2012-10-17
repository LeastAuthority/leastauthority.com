import urllib, time, re
from hashlib import sha1
from base64 import b64encode

from twisted.internet import reactor, task
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


def xml_find(node, *keys):
    r = node.find(keys[0])
    if r is None:
        raise ResponseParseError('Node not found: %r' % (keys,))
    if len(keys) == 1:
        return r
    keys = keys[1:]
    return xml_find(r, *keys)


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
                privatehost = xml_find(inneritem, u'privateDnsName').text
            except ResponseParseError:
                return None

            if not publichost or not privatehost:
                return None

            publichost = publichost.strip()
            privatehost = privatehost.strip()
            m = EC2_PUBLIC_DNS.match(publichost)
            if m:
                # If the name matches EC2_PUBLIC_DNS, we prefer to extract the IP address
                # to eliminate the DNS point of failure.
                publichost = pubIPextractor(publichost)

            addresslist.append( (publichost, privatehost) )
        return addresslist


def pubIPextractor(AWSdnsName):
    if AWSdnsName is None:
        return ""
    assert isinstance(AWSdnsName, str), "AWSdnsName is %s." % AWSdnsName
    AWSdnsName = AWSdnsName.strip()
    publichost = AWSdnsName[len('ec2-'):].split('.')[0].replace('-', '.')
    return publichost


class ServerInfoParser(txaws_ec2_Parser):
    def __init__(self, req_properties, opt_properties=[]):
        self.req_properties = req_properties
        self.opt_properties = opt_properties

    def describe_instances(self, xml_bytes):
        propertytuplelist = []
        doc = xml_parse(xml_bytes)
        node = xml_find(doc, u'reservationSet')
        itemlist = node.findall(u'item')
        for item in itemlist:
            iset = xml_find(item, u'instancesSet')
            inneritem = xml_find(iset, u'item')
            serverproperties = []
            for property in self.req_properties + self.opt_properties:
                try:
                    path = unicode(property).split(u'.')
                    matching = xml_find(inneritem, *path).text
                    if matching is None and property in self.req_properties:
                        return None
                    serverproperties.append(matching)
                except ResponseParseError:
                    return None

            propertytuplelist.append( tuple(serverproperties) )
        return propertytuplelist


def get_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint_uri, parser, *instance_ids):
    """
    Reference: http://docs.amazonwebservices.com/AWSEC2/latest/APIReference/index.html?ApiReference-query-DescribeInstances.html
    """
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=ec2creds, endpoint=endpoint, parser=parser)
    return client.describe_instances(*instance_ids)


class TimeoutError(Exception):
    pass


def wait_for_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint, parser, poll_time, wait_time, stdout, stderr, *instance_ids):
    def _wait(remaining_time):
        d = get_EC2_properties(ec2accesskeyid, ec2secretkey, endpoint, parser, *instance_ids)
        def _maybe_again(res):
            if res:
                return res
            if remaining_time <= 0:
                print >>stdout, "Timed out waiting for EC2 instance addresses."
                raise TimeoutError()
            print >>stdout, "Waiting %d seconds before address request..." % (poll_time,)
            return task.deferLater(reactor, poll_time, _wait, remaining_time - poll_time)
        d.addCallback(_maybe_again)
        return d
    return _wait(wait_time)
