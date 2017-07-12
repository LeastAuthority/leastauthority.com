# Copyright Least Authority Enterprises.
# See LICENSE for details.

from twisted.internet.endpoints import serverFromString
from twisted.application.internet import StreamServerEndpointService
from twisted.web.resource import Resource
from twisted.web.server import Site

from prometheus_client.twisted import MetricsResource


def prometheus_exporter(reactor, port_string):
    """
    Create an ``IService`` that exposes Prometheus metrics from this process
    on an HTTP server on the given port.
    """
    root = Resource()
    root.putChild(b"metrics", MetricsResource())
    service = StreamServerEndpointService(
        serverFromString(reactor, port_string),
        Site(root),
    )
    return service
