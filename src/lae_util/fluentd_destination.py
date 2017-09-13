"""
Fluentd support for Eliot.
"""

from sys import stdout
from io import BytesIO
from json import dumps
from logging import getLogger

import attr
from attr.validators import provides, instance_of

from twisted.python.logfile import LogFile
from twisted.python.filepath import FilePath
from twisted.python.url import URL
from twisted.logger import globalLogPublisher
from twisted.application.service import Service
from twisted.web.iweb import IAgent
from twisted.web.client import (
    HTTPConnectionPool,
    Agent,
    FileBodyProducer,
)
from twisted.web.http_headers import Headers

from eliot import (
    FileDestination,
    add_destination,
    remove_destination,
)

from .eliottools import (
    TwistedLoggerToEliotObserver,
    stdlib_logging_to_eliot_configuration,
)

@attr.s(frozen=True)
class FluentdDestination(object):
    """
    ``FluentdDestination`` is an Eliot log destination which sends logs to a
    Fluentd via the HTTP input plugin.

    .. WARNING:: Combining this with ``_EliotLogging`` will immediately ruin
       your day.
    """
    agent = attr.ib(validator=provides(IAgent))
    fluentd_url = attr.ib(validator=instance_of(URL))

    def _observe(self, message):
        self.agent.request(
            b"POST",
            self.fluentd_url.asURI().asText().encode("ascii"),
            Headers({"Content-Type":["application/x-www-form-urlencoded"]}),
            FileBodyProducer(BytesIO(b"json=" + dumps(message))),
        )

    # Eliot wants this interface.
    __call__ = _observe



def opt_eliot_destination(self, description):
    """
    Add an Eliot logging destination.
    """
    self.setdefault("destinations", []).append(
        _parse_destination_description(description)
    )



class _DestinationParser(object):
    def parse(self, description):
        description = description.decode("ascii")

        kind, args = description.split(":", 1)
        try:
            parser = getattr(self, "_parse_{}".format(kind))
        except AttributeError:
            raise ValueError(
                "Unknown destination description: {}".format(description)
            )
        else:
            return parser(kind, args)


    def _parse_file(self, kind, args):
        if args == "-":
            get_file = lambda: stdout
        else:
            path = FilePath(args)
            get_file = lambda: LogFile(
                path.basename(),
                path.dirname(),
                rotateLength=1024 * 1024 * 1024,
                maxRotatedFiles=10,
            )
        return lambda reactor: FileDestination(get_file())


    def _parse_fluentd_http(self, kind, args):
        return lambda reactor: FluentdDestination(
            # Construct the pool ourselves with the default of using
            # persistent connections to override Agent's default of not using
            # persistent connections.
            agent=Agent(reactor, pool=HTTPConnectionPool(reactor)),
            fluentd_url=URL.fromText(args),
        )


_parse_destination_description = _DestinationParser().parse


class _EliotLogging(Service):
    """
    A service which adds stdout as an Eliot destination while it is running.
    """
    def __init__(self, destinations):
        """
        :param list destinations: The Eliot destinations which will is added by this
            service.
        """
        self.destinations = destinations


    def startService(self):
        self.stdlib_cleanup = stdlib_logging_to_eliot_configuration(getLogger())
        self.twisted_observer = TwistedLoggerToEliotObserver()
        globalLogPublisher.addObserver(self.twisted_observer)

        for dest in self.destinations:
            add_destination(dest)


    def stopService(self):
        self.stdlib_cleanup()
        globalLogPublisher.removeObserver(self.twisted_observer)
        for dest in self.destinations:
            remove_destination(dest)



def eliot_logging_service(reactor, destinations):
    """
    Parse the given Eliot destination descriptions and return an ``IService``
    which will add them when started and remove them when stopped.
    """
    return _EliotLogging(destinations=list(
        get_destination(reactor)
        for get_destination
        in destinations
    ))
