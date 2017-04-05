# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Kubernetes-related functionality used across multiple Python packages.
"""

from functools import partial

from txkube import v1, v1beta1

def derive_pod(deployment, podIP):
    # This is roughly how a Deployment creates a Pod... I suppose.
    return v1.Pod(
        metadata=deployment.metadata.transform(
            [u"name"],
            u"{}-{}".format(deployment.metadata.name, hex(id(deployment))),
        ),
        spec=deployment.spec.template.spec,
        # This is a cheat.  We can't really set the status.  But the
        # in-memory Kubernetes won't set it either so we'd better do it.
        status=v1.PodStatus(
            podIP=podIP,
        ),
    )


def derive_replicaset(deployment):
    return v1beta1.ReplicaSet(
        metadata=v1.ObjectMeta(
            name=u"{}-{}".format(deployment.metadata.name, u"randomsuffix"),
            namespace=deployment.metadata.namespace,
            labels=deployment.metadata.labels,
            annotations=deployment.metadata.annotations,
        ),
        spec=v1beta1.ReplicaSetSpec(
            selector=deployment.spec.selector,
            template=deployment.spec.template,
        ),
    )


def get_replicasets(state, deployment):
    # XXX This ignores namespaces.
    return filter(
        partial(selectors_match_item, deployment.spec.selector),
        state.replicasets.items,
    )


def get_pods(state, deployment):
    # XXX This ignores namespaces.
    return filter(
        partial(selectors_match_item, deployment.spec.selector),
        state.pods.items,
    )


def selectors_match_item(selector, item):
    return selectors_match_metadata(selector, item.metadata)


def selectors_match_metadata(selector, metadata):
    if selector.matchExpressions is not None:
        raise NotImplementedError("selectors_match_metadata does not implement matchExpressions")

    if selector.matchLabels is None:
        return True

    return all(
        metadata.labels.get(key) == value
        for (key, value)
        in selector.matchLabels.iteritems()
    )
