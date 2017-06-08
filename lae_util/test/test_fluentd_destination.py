
from __future__ import unicode_literals

from twisted.python.url import URL
from twisted.web.resource import Resource
from twisted.web.server import Site
from twisted.web.client import Agent
from twisted.internet.task import deferLater
from twisted.trial.unittest import TestCase

from ..fluentd_destination import FluentdDestination


class Collector(Resource):
    def __init__(self):
        Resource.__init__(self)
        self.collected = []


    def render_POST(self, request):
        self.collected.append(request.content.read())
        return b""



class FluentdDestinationTests(TestCase):
    def test_posted(self):
        root = Resource()
        collector = Collector()
        root.putChild(b"foo", collector)

        from twisted.internet import reactor
        while True:
            try:
                port = reactor.listenTCP(0, Site(root))
            except:
                pass
            else:
                self.addCleanup(port.stopListening)
                port_number = port.getHost().port
                break

        fluentd_url = URL(
            scheme="http",
            host="127.0.0.1",
            port=port_number,
            path=["foo"],
        )

        agent = Agent(reactor)
        destination = FluentdDestination(agent, fluentd_url)
        destination({"hello": "world"})

        def check():
            self.assertEquals(collector.collected, [b'json={"hello": "world"}'])

        return deferLater(reactor, 0.1, check)
