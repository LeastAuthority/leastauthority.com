# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Monitor a transfer rate to and from a Tahoe-LAFS storage grid.
"""

from functools import partial

import attr

from twisted.logger import Logger
from twisted.python.usage import Options
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
    retry_failure, backoff,
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
    _when = attr.ib()


    def set(self):
        self._when = self.clock.seconds()
        _logger.info(
            format="setting check time to {check_time}",
            check_time=self._when,
        )


    def get(self):
        return self._when



def makeService(options):
    from twisted.internet import reactor

    service = MultiService()

    last_progress = _CheckTime(clock=reactor, when=reactor.seconds())

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
            last_progress.set,
        )
    ).setServiceParent(service)

    check_liveness = partial(
        is_alive,
        reactor,
        last_progress.get,
        options["interval"] * 3,
    )

    options.get_metrics_service(reactor).setServiceParent(service)
    options.get_liveness_service(reactor, check_liveness).setServiceParent(service)

    return service


_logger = Logger()

def is_alive(clock, last_progress, maximum_age):
    now = clock.seconds()
    progress = last_progress()
    age = now - progress
    _logger.info(
        format="checking liveness",
        now=now,
        progress=progress,
        age=age,
        maximum_age=maximum_age,
    )
    return age < maximum_age


def _timer_service(reactor, *a, **kw):
    service = TimerService(*a, **kw)
    service.clock = reactor
    return service



def _create_monitor_service(
        reactor, interval, introducer_furl, mutable_file_cap, configuration,
        progress_callback,
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
            _measure,
            reactor,
            lafs,
            mutable_file_cap,
            progress_callback,
        ),
    )
    return d



def _measure(reactor, lafs, mutable_file_cap, progress_callback):
    print("Measuring")
    d = retry_failure(
        reactor,
        lambda: roundtrip_check(lafs, [mutable_file_cap], progress_callback),
        expected=(Exception,),
        steps=backoff(
            step=1.0,
            maximum_step=5.0,
            timeout=30.0,
        ),
    )
    d.addCallback(lambda ignored: _logger.info("measurement completed"))
    d.addErrback(partial(_logger.failure, "measurement failure"))
    return d



def _call_and_passthrough(f):
    def g(result):
        f()
        return result
    return g
