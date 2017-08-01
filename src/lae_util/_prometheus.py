# Copyright Least Authority Enterprises.
# See LICENSE for details.

from __future__ import unicode_literals

from twisted.logger import globalLogPublisher
from twisted.internet.endpoints import serverFromString
from twisted.application.service import MultiService, Service
from twisted.application.internet import StreamServerEndpointService
from twisted.web.resource import Resource
from twisted.web.server import Site

from prometheus_client import Counter
from prometheus_client.twisted import MetricsResource

_UNHANDLED_ERRORS = Counter(
    "s4_unhandled_error_counter",
    "Total S4 Unhandled Errors",
)



def get_metrics_service(options, reactor):
    return prometheus_exporter(reactor, options["metrics-port"])



def opt_metrics_port(cls):
    cls.optParameters = list(cls.optParameters) + [
        ("metrics-port", None, b"tcp:9000",
         "A server endpoint description string on which to run a metrics-exposing server.",
        ),
    ]
    cls.get_metrics_service = get_metrics_service
    return cls



def prometheus_exporter(reactor, port_string):
    """
    Create an ``IService`` that exposes Prometheus metrics from this process
    on an HTTP server on the given port.
    """
    parent = MultiService()

    root = Resource()
    root.putChild(b"metrics", MetricsResource())
    StreamServerEndpointService(
        serverFromString(reactor, port_string),
        Site(root),
    ).setServiceParent(parent)

    _ExtraMetrics(
    ).setServiceParent(parent)

    return parent



class _ExtraMetrics(Service):
    """
    Collect additional metrics to be published.
    """
    def startService(self):
        globalLogPublisher.addObserver(_count_errors)
        return Service.startService(self)


    def stopService(self):
        globalLogPublisher.removeObserver(_count_errors)
        return Service.stopService(self)



def _count_errors(event):
    """
    Observe any Deferreds that are garbage collected with unhandled failures.
    Translate these observations into a count of such events.
    """
    if _is_unhandled_deferred_error(event):
        _UNHANDLED_ERRORS.inc()



def _is_unhandled_deferred_error(event):
    """
    Determine whether a log event represents a failure unhandled by a
    Deferred.
    """
    return (
        event.get("log_namespace", None) == "twisted.internet.defer"
    ) and (
        event.get("log_format", None) == "Unhandled error in Deferred"
    )
