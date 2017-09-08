# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Monitor a transfer rate to and from a Tahoe-LAFS storage grid.
"""

from functools import partial

import attr

from twisted.python.usage import Options
from twisted.internet.defer import maybeDeferred
from twisted.internet.endpoints import serverFromString
from twisted.application.service import MultiService
from twisted.application.internet import (
    StreamServerEndpointService,
    TimerService,
)
from twisted.web.server import Site
from twisted.web.resource import Resource

from lae_util import (
    opt_metrics_port,
    AsynchronousService,
)

from lae_util.tahoe import (
    create_tahoe_lafs_client,
    roundtrip_check,
)



class LivenessResource(Resource):
    def __init__(self, check):
        Resource.__init__(self)
        self._check = check


    def render_GET(self, request):
        if self._check():
            return b""
        request.setResponseCode(500)
        return b""



def get_liveness_service(options, reactor, check_liveness):
    root = Resource()
    root.putChild(b"liveness", LivenessResource(check_liveness))

    return StreamServerEndpointService(
        serverFromString(reactor, options["liveness-port"]),
        Site(root),
    )


def opt_liveness_probe(cls):
    cls.optParameters = list(cls.optParameters) + [
        ("liveness-port", None, b"tcp:9001",
         "A server endpoint description string on which to run a liveness probe server.",
        ),
    ]
    cls.get_liveness_service = get_liveness_service
    return cls


@opt_liveness_probe
@opt_metrics_port
class Options(Options):
    optParameters = [
        ("introducer-furl", None, None,
         "The fURL of a Tahoe-LAFS introducer to use to connect to storage servers.",
         unicode,
        ),
        ("scratch-cap", None, None,
         "The Tahoe-LAFS mutable file capability to scribble on for measurements.",
         unicode,
        ),
        ("interval", None, 30.0,
         "The amount of time to allow to pass between measurements.",
         float,
        ),
        ("shares-needed", None, 1, None, int),
        ("shares-happy", None, 1, None, int),
        ("shares-total", None, 1, None, int),
    ]



@attr.s
class _CheckTime(object):
    clock = attr.ib()
    when = attr.ib()


    def set(self):
        self._when = self.clock.seconds()


    def get(self):
        return self._when



def makeService(options):
    from twisted.internet import reactor

    service = MultiService()

    last_check = _CheckTime(reactor, reactor.seconds())

    AsynchronousService(
        lambda: _create_monitor_service(
            reactor,
            options["interval"],
            options["introducer-furl"],
            options["scratch-cap"],
            dict(
                shares_needed=options["shares-needed"],
                shares_happy=options["shares-happy"],
                shares_total=options["shares-total"],
            ),
            last_check.set,
        )
    ).setServiceParent(service)

    check_liveness = partial(
        is_alive,
        reactor,
        last_check.get,
        options["interval"] * 3,
    )

    options.get_metrics_service(reactor).setServiceParent(service)
    options.get_liveness_service(reactor, check_liveness).setServiceParent(service)

    return service



def is_alive(clock, last_check, maximum_age):
    return clock.seconds() - last_check() < maximum_age



def _timer_service(reactor, *a, **kw):
    service = TimerService(*a, **kw)
    service.clock = reactor
    return service



def _create_monitor_service(
        reactor, interval, introducer_furl, mutable_file_cap, configuration,
        set_check_time,
):
    d = create_tahoe_lafs_client(
        reactor,
        introducer_furl=introducer_furl,
        **configuration
    )
    d.addCallback(
        lambda lafs: _timer_service(
            reactor,
            interval,
            _record_success(_measure, set_check_time),
            lafs,
            mutable_file_cap,
        ),
    )
    return d



def _measure(lafs, mutable_file_cap):
    print("Measuring")
    return roundtrip_check(lafs, [mutable_file_cap])



def _call_and_passthrough(f):
    def g(result):
        f()
        return result
    return g



def _record_success(f, record):
    def g(*a, **kw):
        d = maybeDeferred(f, *a, **kw)
        d.addCallback(_call_and_passthrough(record))
        return d
    return g
