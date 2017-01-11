# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
An implementation of txACME's certificate storage interface backed
on to Kubernetes secrets.
"""

from json import dumps, loads
from io import BytesIO

import attr
from attr import validators

from pem import parse

from zope.interface import implementer

from twisted.python.failure import Failure
from twisted.web.iweb import IAgent
from twisted.web.http import OK
from twisted.web.http_headers import Headers
from twisted.web.client import FileBodyProducer, Agent, readBody
from twisted.internet import task

from txacme.interfaces import ICertificateStore

@implementer(ICertificateStore)
@attr.s(frozen=True)
class KubernetesSecretsCertificateStore(object):
    agent = attr.ib(validator=validators.provides(IAgent))
    namespace = attr.ib(validator=validators.instance_of(unicode))
    name = attr.ib(validator=validators.instance_of(unicode))
    kubernetes = attr.ib(default=u"kubernetes")
    cooperator = attr.ib(default=task)

    @property
    def _url(self):
        return u"http://{kubernetes}/api/v1/namespaces/{namespace}/secrets/{name}".format(
            **attr.asdict(self)
        ).encode("ascii")


    def _producer(self, document):
        return FileBodyProducer(BytesIO(dumps(document)), cooperator=self.cooperator)


    def _create(self):
        body = self._producer({
            u"apiVersion": u"v1",
            u"kind": u"Secret",
            u"metadata": {
                u"name": self.name,
            },
            u"type": u"Opaque",
            u"data": {
            },
        })
        return self.agent.request(b"POST", self._url.rsplit(b"/", 1)[0], None, body)


    def get(self, server_name):
        d = self.as_dict()
        d.addCallback(itemgetter(server_name))
        return d


    def store(self, server_name, pem_objects):
        headers = Headers({u"content-type": [u"application/strategic-merge-patch+json"]})
        patch = {
            u"data": {
                server_name: b"".join(obj.as_bytes() for obj in pem_objects).encode("base64"),
            },
        }
        body = self._producer(patch)
        d = self.agent.request(b"PATCH", self._url, headers, body)
        d.addCallback(require_status(OK))
        return d


    def as_dict(self):
        d = self.agent.request(b"GET", self._url)
        d.addCallback(require_status(OK))
        d.addCallback(readBody)
        def decode(body):
            secret = loads(body)
            print(secret)
            certs = secret.get(u"data", {})
            return {
                name: encoded.decode("base64")
                for (name, encoded)
                in certs.iteritems()
            }
        d.addCallback(decode)
        return d


def require_status(*statuses):
    def check(response):
        if response.code in statuses:
            return response
        return readBody(response).addCallback(
            lambda body: Failure(Exception(response.code, body)),
        )
    return check
