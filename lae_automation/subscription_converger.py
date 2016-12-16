"""
This module implements a convergence service which keeps
Kubernetes configuration in line with customer subscriptions.
"""

from eliot import startAction

from twisted.python.usage import Options as _Options
from twisted.web.client import Agent

class Options(_Options):
    optParameters = [
        ("endpoint", "e", None, "The root URL of the subscription manager service."),
    ]

def makeService(options):
    from twisted.internet import reactor
    agent = Agent(reactor)
    subscription_client = SMClient(endpoint=options["endpoint"], agent=agent)

    k8s_client = pykube.HTTPClient.from_service_account()

    return TimerService(
        1.0,
        divert_errors_to_log(converge), subscription_client, k8s_client,
    )

def divert_errors_to_log(f):
    def g(*a, **kw):
        action = startAction("subscription_converger:" + f.__name__)
        with action.context():
            d = DeferredContext(maybeDeferred(f, *a, **kw))
            d.addFinishAction()
            # The failure was logged by the above.  Now squash it.
            d.addErrback(lambda err: None)
            return d.result
    return g

def converge(subscriptions, k8s):
    desired = subscriptions.list().addCallback(set)
    service = get_subscription_service(k8s)

    d = DeferredList([desired, service])
    d.addCallback(converge_logic)
    d.addCallback(enact_configuration, k8s)
    return d


def converge_service(desired, service):
    actual = get_configured_subscriptions(service)
    changes = compute_changes(desired, actual)
    # XXX Cannot update configuration without retrieving more state.
    new_service = update_configuration(changes, service)
    return new_service



def compute_changes(desired, service):
    actual = 
    extra = actual - desired
    missing = desired - actual

    return dict(
        changes=map(Create, extra) + map(Delete, missing),
        service=service,
    )

@attr.s(frozen=True)
class Create(object):
    subscription = attr.ib()


def update_configuration(computed):
    changes = computed["changes"]
    service = computed["service"]

    for change in changes:
        service = change.enact(service)

    return service

def enact_configuration(service, k8s):
    # XXX No such API
    k8s.replace(service)
