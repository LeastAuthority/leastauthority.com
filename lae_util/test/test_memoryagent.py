# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Tests for ``lae_automation.memoryagent``.
"""

from io import BytesIO
from urllib import quote

from hypothesis import given
from hypothesis.strategies import none, text, integers, lists, dictionaries, sampled_from

from testtools.matchers import Equals

from twisted.web.iweb import IResponse
from twisted.web.http_headers import Headers
from twisted.web.resource import Resource
from twisted.web.client import FileBodyProducer, IAgent, readBody

from lae_util.testtools import TestCase
from lae_util.testtools.matchers import Implements, Provides
from lae_util.testtools.strategies import path_segments

from lae_util.uncooperator import Uncooperator
from lae_util.memoryagent import MemoryAgent


class _nothing(object):
    """
    No supplied value - useful primarily for the nicer way it repr()s
    and because it is distinct from None.
    """
    def __repr__(self):
        return "<nothing>"
nothing = _nothing()


class Recorder(Resource):
    """
    A resource which records the parameters of the request which is
    dispatched to it.  Tests can then make assertions about the
    recorded state.
    """
    method = nothing
    headers = nothing
    body = nothing

    def render(self, request):
        self.method = request.method
        self.headers = request.requestHeaders
        self.body = request.content.read()
        return b""


class Player(Resource):
    """
    A resource which spits out statically configured responses.  This
    helps tests make assertions about response handling.
    """
    def __init__(self, code, message, headers, body):
        Resource.__init__(self)
        self.code = code
        self.message = message
        self.headers = headers
        self.body = body

    def render(self, request):
        request.setResponseCode(self.code, self.message)
        for k, vs in self.headers.getAllRawHeaders():
            request.responseHeaders.setRawHeaders(k, vs)
        return self.body


def http_methods():
    """
    Strategy for generating the HTTP request verbs/methods.
    """
    return sampled_from((b"GET", b"POST", b"DELETE", b"PUT", b"HEAD"))


def http_codes():
    """
    Strategy for generating HTTP response code integers.

    Currently nothing below 200 is generated because 1xx codes have
    some additional, complicated semantics.
    """
    return integers(min_value=200, max_value=599)


def http_messages():
    """
    Strategy for generating alternate HTTP response messages/phrases.
    """
    return text().map(lambda x: x.encode("utf-8"))

def http_headers():
    """
    Strategy for generating ``Headers`` populated with random HTTP
    headers.

    This could probably use some more work.
    """
    return dictionaries(
        keys=sampled_from((
            b"accept",
            b"accept-charset",
            b"accept-encoding",
            b"accept-language",
            b"accept-ranges",
            b"age",
            b"allow",
            b"authorization",
            b"cache-control",
            b"connection",
            b"content-encoding",
            b"content-language",
            # XXX The rest, I guess, plus randomly generate some?
        )),
        values=text().map(lambda x: x.encode("utf-8")),
    ).map(
        lambda h: Headers({k: [v] for (k, v) in h.items()})
    )

def http_bodies():
    """
    Strategy for generating some UTF-8 bytes usable as a request or
    response body.
    """
    return text().map(lambda x: x.encode("utf-8"))


class MemoryAgentTests(TestCase):
    """
    Tests for ``MemoryAgent``.
    """
    def test_interface(self):
        """
        ``MemoryAgent`` implements ``IAgent``.
        """
        self.assertThat(MemoryAgent, Implements(IAgent))


    @given(
        method=http_methods(),
        path_segments=lists(elements=path_segments(), min_size=1),
        headers=http_headers(),
        body=http_bodies(),
    )
    def test_request(self, method, path_segments, headers, body):
        """
        ``MemoryAgent`` uses the ``IResource`` it is given as the root of
        a resource hierarchy and finds the correct child to which to
        deliver the requests represented ``MemoryAgent.request``
        calls.

        The request path is tested by selection of the correct
        ``IResource``.  The request method, headers, and body are
        expected to be delivered to the resource exactly as specified
        in the ``MemoryAgent.request`` call.
        """
        child = recorder = Recorder()
        parent = None
        for segment in path_segments[::-1]:
            parent = Resource()
            parent.putChild(segment, child)
            child = parent

        producer = FileBodyProducer(
            BytesIO(body), cooperator=Uncooperator()
        )

        path = b"/" + b"/".join(quote(segment, safe=b"") for segment in path_segments)
        agent = MemoryAgent(parent)
        response = self.successResultOf(
            agent.request(method, path, headers, producer)
        )

        self.expectThat(recorder.method, Equals(method))
        self.expectThat(recorder.headers, Equals(headers))
        self.expectThat(recorder.body, Equals(body))

    @given(
        code=http_codes(),
        # Technical the message can be empty but the Twisted API
        # replaces empty messages with the default message for the
        # code.  Avoid tripping over that case.
        message=http_messages().filter(len),
        headers=http_headers(),
        body=http_bodies()
    )
    def test_response(self, code, message, headers, body):
        """
        ``MemoryAgent.request`` returns a ``Deferred`` which fires with an
        ``IResponse`` provider containing the response code, message,
        headers, and body generated by the ``IResource`` to which the
        request was dispatched.
        """
        player = Player(code, message, headers, body)
        root = Resource()
        root.putChild("", player)

        agent = MemoryAgent(root)
        response = self.successResultOf(agent.request(b"GET", b"/", Headers()))

        self.expectThat(response, Provides(IResponse))
        self.expectThat(response.code, Equals(code))
        self.expectThat(response.phrase, Equals(message))
        self.expectThat(response.headers, Equals(headers))
        self.expectThat(response.length, Equals(len(body)))
        self.expectThat(self.successResultOf(readBody(response)), Equals(body))
