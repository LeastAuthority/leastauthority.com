from json import loads
from datetime import datetime

from testtools.matchers import Equals, MatchesDict

from twisted.test.proto_helpers import StringTransport
from twisted.internet.address import IPv4Address
from twisted.web.http import HTTPChannel, Request, datetimeToLogString

from .. import _LogFormatter

from lae_util.testtools import TestCase


class JSONAccessLogTests(TestCase):
    """
    Tests for ``_LogFormatter``.
    """
    def test_formatting(self):
        """
        ``_LogFormatter.json_access_log`` returns a JSON-encoded object with the
        usual http access log information as properties.
        """
        when = 123456789
        json_access_log = _LogFormatter(
            lambda: datetime.utcfromtimestamp(when),
        ).json_access_log

        ip = "192.0.2.1"
        channel = HTTPChannel()
        transport = StringTransport(peerAddress=IPv4Address("TCP", ip, 12345))
        channel.makeConnection(transport)
        request = Request(channel)
        request.gotLength(None)
        request.requestReceived("GET", "/", "HTTP/1.1")
        event = json_access_log(datetimeToLogString(when), request)
        self.assertThat(
            loads(event),
            MatchesDict(dict(
                timestamp=Equals("1973-11-29T21:33:09"),
                ip=Equals(ip),
                method=Equals("GET"),
                uri=Equals("/"),
                protocol=Equals("HTTP/1.1"),
                code=Equals(200),
                length=Equals(None),
                referrer=Equals(None),
                agent=Equals(None),
            )),
        )
