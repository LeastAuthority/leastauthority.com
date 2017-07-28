# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Monitor a transfer rate to and from a Tahoe-LAFS storage grid.
"""

from twisted.python.usage import Options
from twisted.application.service import MultiService
from twisted.application.internet import TimerService

from lae_util import (
    prometheus_exporter,
    AsynchronousService,
)

from lae_util.tahoe import (
    create_tahoe_lafs_client,
    roundtrip_check,
)


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
        ("metrics-port", None, "tcp:9000",
         "A server endpoint description string on which to run a metrics-exposing server.",
        ),
    ]



def makeService(options):
    from twisted.internet import reactor

    service = MultiService()

    AsynchronousService(
        lambda: _create_monitor_service(
            reactor,
            options["interval"],
            options["introducer-furl"],
            options["scratch-cap"],
        )
    ).setServiceParent(service)

    prometheus_exporter(
        reactor, options["metrics-port"],
    ).setServiceParent(service)

    return service



def _timer_service(reactor, *a, **kw):
    service = TimerService(*a, **kw)
    service.clock = reactor
    return service



def _create_monitor_service(reactor, interval, introducer_furl, mutable_file_cap):
    d = create_tahoe_lafs_client(
        reactor,
        introducer_furl=introducer_furl,
    )
    d.addCallback(
        lambda lafs: _timer_service(
            reactor, interval, _measure, lafs, mutable_file_cap,
        ),
    )
    return d



def _measure(lafs, mutable_file_cap):
    print("Measuring")
    return roundtrip_check(lafs, [mutable_file_cap])
