
from json import loads, dumps
from twisted.python.url import URL
from twisted.web.http import OK
from twisted.web.client import readBody

import attr

from eliot import start_action
from eliot.twisted import DeferredContext

from txkube import (
    v1, v1beta1, network_kubernetes, authenticate_with_serviceaccount,
    iobject_to_raw, iobject_from_raw,
)
from txkube._network import (
    _BytesProducer, check_status, object_location, log_response_object,
)


@attr.s(frozen=True)
class LabelSelector(object):
    labels = attr.ib()

    def match(self, obj):
        missing = object()
        return {
            key: obj["metadata"]["labels"].get(key, missing)
            for key in self.labels
        } == self.labels



class NullSelector(object):
    def match(self, obj):
        return True



def select(collection, selector):
    for obj in collection.items:
        if selector.match(obj):
            yield obj



@attr.s(frozen=True)
class KubeClient(object):
    k8s = attr.ib()

    @classmethod
    def from_service_account(cls):
        from twisted.internet import reactor
        kubernetes = network_kubernetes(
            base_url=URL.fromText(u"https://kubernetes/"),
            agent=authenticate_with_serviceaccount(reactor),
        )
        client = kubernetes.client()
        return cls(k8s=client)

    def select(self, kind, selector):
        return self.k8s.list(kind).addCallback(select, selector)

    def get_configmaps(self, selector=NullSelector()):
        return self.select(v1.ConfigMap, selector)

    def get_services(self, selector=NullSelector()):
        return self.select(v1.Service, selector)

    def get_deployments(self, selector=NullSelector()):
        return self.select(v1beta1.Deployment, selector)

    def delete(self, obj):
        return self.k8s.delete(obj)

    def create(self, obj):
        return self.k8s.create(obj)

    def replace(self, obj):
        return self.k8s.replace(obj)
