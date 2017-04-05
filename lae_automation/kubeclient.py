# Copyright Least Authority Enterprises.
# See LICENSE for details.

from twisted.python.url import URL

import attr

from txkube import (
    v1, v1beta1, network_kubernetes, authenticate_with_serviceaccount,
)

@attr.s(frozen=True)
class LabelSelector(object):
    labels = attr.ib()

    def match(self, obj):
        missing = object()
        return {
            key: obj.metadata.labels.get(key, missing)
            for key in self.labels
        } == self.labels



class NullSelector(object):
    def match(self, obj):
        return True



@attr.s(frozen=True)
class And(object):
    selectors = attr.ib()

    def match(self, obj):
        return all(s.match(obj) for s in self.selectors)



@attr.s(frozen=True)
class NamespaceSelector(object):
    namespace = attr.ib()

    def match(self, obj):
        return self.namespace == obj.metadata.namespace



def select(collection, selector):
    return filter(selector.match, collection.items)



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

    def get_replicasets(self, selector=NullSelector()):
        return self.select(v1beta1.ReplicaSet, selector)

    def get_pods(self, selector=NullSelector()):
        return self.select(v1.Pod, selector)

    def delete(self, obj):
        return self.k8s.delete(obj)

    def create(self, obj):
        return self.k8s.create(obj)

    def replace(self, obj):
        return self.k8s.replace(obj)
