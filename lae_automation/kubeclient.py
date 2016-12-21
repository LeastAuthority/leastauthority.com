import pykube
import attr

@attr.s(frozen=True)
class KubeClient(object):
    k8s = attr.ib()

    @classmethod
    def from_service_account(cls):
        return cls(k8s=pykube.HTTPClient.from_service_account())

    def _get(self, kind, selectors):
        return kind.objects(self.k8s).filter(**selectors)
    
    def get_configmap(self, selectors):
        return self._get(pykube.ConfigMap, selectors)
    
    def get_services(self, selectors):
        return self._get(pykube.Service, selectors)

    def get_deployments(self, selectors):
        return self._get(pykube.Deployment, selectors)

    def destroy(self, kind, name):
        pass

    def create(self, definition):
        pass

    def apply(self, definition):
        pass
