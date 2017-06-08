"""
Fluentd support for Eliot.
"""

from io import BytesIO
from json import dumps

import attr
from attr.validators import provides, instance_of

from twisted.python.url import URL
from twisted.web.iweb import IAgent
from twisted.web.client import FileBodyProducer


@attr.s(frozen=True)
class FluentdDestination(object):
    agent = attr.ib(validator=provides(IAgent))
    fluentd_url = attr.ib(validator=instance_of(URL))

    def _observe(self, message):
        self.agent.request(
            b"POST",
            self.fluentd_url.asURI().asText().encode("ascii"),
            None,
            FileBodyProducer(BytesIO(b"json=" + dumps(message))),
        )

    # Eliot wants this interface.
    __call__ = _observe
