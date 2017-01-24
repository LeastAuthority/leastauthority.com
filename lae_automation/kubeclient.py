
from txkube import (
    ConfigMap,

    network_kubernetes,
)

import attr


def selectors_match(obj, selectors):
    missing = object()
    return {
        key: obj["metadata"]["labels"].get(key, missing)
        for key in selectors
    } == selectors


@attr.s(frozen=True)
class KubeClient(object):
    k8s = attr.ib()

    @classmethod
    def from_service_account(cls):
        from twisted.internet import reactor
        kubernetes = network_client(
            base_url=URL.fromText(u"https://kubernetes/"),
            agent=authenticate_with_service_account(reactor),
        )
        client = kubernetes.client()
        return cls(k8s=client)

    def _match(self, obj, selectors):
        if selectors is None:
            return True
        return selectors_match(obj, selectors)

    def _get(self, kind, selectors):
        d = self.k8s.list(kind)
        def got_objs(collection):
            for obj in collection.items:
                if self._match(obj, selectors):
                    yield obj
        d.addCallback(got_objs)
        return d

    def get_configmaps(self, selectors):
        return self._get(ConfigMap, selectors)

    def get_services(self, selectors):
        return self._get(Service, selectors)

    def get_deployments(self, selectors):
        return self._get(Deployment, selectors)

    def destroy(self, obj):
        return self.k8s.destroy(obj)

    def create(self, obj):
        return self.k8s.create(obj)

    def apply(self, definition):
        pass
