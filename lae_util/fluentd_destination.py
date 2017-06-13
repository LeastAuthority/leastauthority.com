"""
Fluentd support for Eliot.
"""

from sys import stdout
from io import BytesIO
from json import dumps

import attr
from attr.validators import provides, instance_of

from twisted.python.url import URL
from twisted.application.service import Service
from twisted.web.iweb import IAgent
from twisted.web.client import Agent
from twisted.web.client import FileBodyProducer
from twisted.web.http_headers import Headers

from eliot import (
    FileDestination,
    add_destination,
    remove_destination,
)

@attr.s(frozen=True)
class FluentdDestination(object):
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



def _parse_destination_description(description):
    if description == "stdout":
        return lambda reactor: FileDestination(stdout)
    if description.startswith("fluentd:"):
        return lambda reactor: FluentdDestination(
            agent=Agent(reactor),
            fluentd_url=URL.fromText(
                description[len("fluentd:"):].decode("ascii"),
            )
        )
    raise ValueError(
        "Unknown destination description: {}".format(description)
    )



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
        for dest in self.destinations:
            add_destination(dest)


    def stopService(self):
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
