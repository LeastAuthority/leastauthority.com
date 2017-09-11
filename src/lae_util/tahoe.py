# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Utilities for managing a Tahoe-LAFS client node and interacting with its
web API.
"""

from io import BytesIO
from os import environ
from time import time
from tempfile import mkdtemp
from random import randrange
from functools import partial

from twisted.python.filepath import FilePath
from twisted.internet.defer import inlineCallbacks, succeed
from twisted.internet.interfaces import IProcessTransport
from twisted.internet.protocol import ProcessProtocol
from twisted.internet.utils import getProcessOutput
from twisted.internet.task import deferLater
from twisted.web.client import FileBodyProducer

import treq as _treq

import attr
from attr.validators import optional, instance_of, provides

from hyperlink import URL

from prometheus_client import Counter, Gauge

from allmydata.util.configutil import (
    get_config,
    set_config,
    write_config,
)


@attr.s
class _TahoeConfiguration(object):
    introducer_furl = attr.ib(validator=instance_of(unicode))
    shares_needed = attr.ib(validator=instance_of(int), default=1)
    shares_happy = attr.ib(validator=instance_of(int), default=1)
    shares_total = attr.ib(validator=instance_of(int), default=1)

    def _create_client_node_dir(self, reactor, client_node_dir):
        d = getProcessOutput(
            u"tahoe", (
                u"create-client",
                u"--introducer", self.introducer_furl,
                u"--shares-needed", unicode(self.shares_needed),
                u"--shares-happy", unicode(self.shares_happy),
                u"--shares-total", unicode(self.shares_total),
                client_node_dir.path,
            ),
            env=environ,
        )
        return d


    def _rewrite_web_port(self, client_node_dir):
        port_number = randrange(20000, 30000)
        cfg_path = client_node_dir.child(u"tahoe.cfg").path
        cfg = get_config(cfg_path)
        set_config(
            cfg,
            u"node",
            u"web.port",
            u"tcp:{}:interface=127.0.0.1".format(port_number),
        )
        write_config(cfg_path, cfg)
        return port_number


    def instantiate(self, reactor, client_node_dir):
        d = self._create_client_node_dir(reactor, client_node_dir)
        def created(ignored):
            port_number = self._rewrite_web_port(client_node_dir)
            return _TahoeClient(
                self,
                client_node_dir,
                URL(u"http", u"127.0.0.1", port=port_number),
            )
        d.addCallback(created)
        return d



@attr.s
class _TahoeClient(object):
    configuration = attr.ib(validator=optional(instance_of(_TahoeConfiguration)))
    root = attr.ib(validator=optional(instance_of(FilePath)))
    root_uri = attr.ib(validator=instance_of(URL))

    def launch(self, reactor):
        process = reactor.spawnProcess(
            ProcessProtocol(),
            u"tahoe", (
                u"tahoe",
                u"run",
                self.root.path,
            ),
            env=environ,
        )
        return succeed(_TahoeProcess(self, process))



@attr.s(frozen=True)
class _TahoeProcess(object):
    _client = attr.ib(validator=instance_of(_TahoeClient))
    _process = attr.ib(validator=provides(IProcessTransport))

    def stop(self):
        self._process.signalProcess("KILL")
        return succeed(None)



class _ReadProgressFileWrapper(object):
    def __init__(self, wrapped, progress):
        self._wrapped = wrapped
        self._progress = progress
        self.seek = wrapped.seek
        self.tell = wrapped.tell
        self.close = wrapped.close


    def read(self, length):
        result = self._wrapped.read(length)
        self._progress()
        return result



@attr.s(frozen=True)
class _LAFS(object):
    tahoe_client = attr.ib(validator=instance_of(_TahoeClient))
    tahoe_process = attr.ib(validator=optional(instance_of(_TahoeProcess)))
    treq = attr.ib()

    def shutdown(self):
        if self.tahoe_process is not None:
            return self.tahoe_process.stop()


    @property
    def _root_uri(self):
        return self.tahoe_client.root_uri


    def _put_file(self, uri, contents, progress_callback):
        uri_bytes = uri.to_uri().to_text().encode("ascii")
        print("PUT {}".format(uri_bytes))
        d = self.treq.put(
            uri_bytes,
            FileBodyProducer(
                _ReadProgressFileWrapper(
                    BytesIO(contents),
                    progress_callback,
                ),

            ),
        )
        d.addCallback(self.treq.text_content)
        return d


    def create_mutable_file(self, contents, format=u"SDMF"):
        uri = self._root_uri.child(u"uri").add(u"format", format)
        return self._put_file(uri, contents, lambda: None)


    def write_mutable_file(self, loc, contents, progress_callback):
        uri = self._root_uri.child(u"uri", *loc)
        return self._put_file(uri, contents, progress_callback)


    def read_file(self, loc, progress_callback):
        uri = self._root_uri.child(u"uri", *loc)
        uri_bytes = uri.to_uri().to_text().encode("ascii")
        print("GET {}".format(uri_bytes))
        d = self.treq.get(uri_bytes)
        d.addCallback(
            partial(content_with_progress, progress_callback, self.treq),
        )
        return d



def content_with_progress(progress_callback, treq, response):
    accumulator = []
    def accumulate_and_report(chunk):
        accumulator.append(chunk)
        progress_callback()

    d = treq.collect(response, accumulate_and_report)
    d.addCallback(lambda ignored: b"".join(accumulator))
    return d



def join_tahoe_lafs_client(client_directory, treq=_treq):
    config = get_config(client_directory.child(u"tahoe.cfg").path)
    server_endpoint = config.get("node", "web.port")
    root_uri = URL(u"http", u"127.0.0.1", port=int(server_endpoint.split(":")[1]))
    client = _TahoeClient(
        configuration=None,
        root=client_directory,
        root_uri=root_uri,
    )
    return _LAFS(
        tahoe_client=client,
        tahoe_process=None,
        treq=_treq,
    )


def create_tahoe_lafs_client(reactor, treq=_treq, **configuration):
    cfg = _TahoeConfiguration(**configuration)
    client_node_dir = FilePath(mkdtemp(suffix=u".tahoe"))
    d = cfg.instantiate(reactor, client_node_dir)
    def instantiated(tahoe_client):
        d = tahoe_client.launch(reactor)
        def launched(tahoe_process):
            d = _wait_until_reachable(reactor, treq, tahoe_client.root_uri)

            # No easy way to know when a storage server connection has
            # actually been established.  Hack this with a fixed delay for
            # now.
            d.addCallback(lambda ignored: deferLater(reactor, 10, lambda: None))

            d.addCallback(lambda ignored: _LAFS(tahoe_client, tahoe_process, treq))
            return d
        d.addCallback(launched)
        return d
    d.addCallback(instantiated)
    return d



def _wait_until_reachable(reactor, treq, uri):
    d = treq.get(uri.to_uri().to_text().encode("ascii"))
    d.addErrback(
        lambda ignored: deferLater(
            reactor,
            3, lambda: _wait_until_reachable(reactor, treq, uri),
        ),
    )
    return d


def roundtrip_check(lafs, mutable_file_loc, progress_callback):
    pattern = u"{:04x}".format(randrange(2 ** 16)).encode("ascii")
    contents = (16 * 1024 * 1024) / len(pattern) * pattern
    size = len(contents)
    d = _measure_async_time(
        lambda interval: _LAST_WRITE.set(size / interval),
        lafs.write_mutable_file(mutable_file_loc, contents, progress_callback),
    )

    def check(result):
        if contents == result:
            _ROUNDTRIP_SUCCESS.inc()
        else:
            raise Exception("mismatch response bytes:\n{}".format(result))

    def wrote(ignored):
        d = _measure_async_time(
            lambda interval: _LAST_READ.set(size / interval),
            lafs.read_file(mutable_file_loc, progress_callback).addCallback(check),
        )
        return d
    d.addCallback(wrote)

    def failed(reason):
        _ROUNDTRIP_FAILURE.inc()
        return reason
    d.addErrback(failed)
    return d



def _measure_async_time(observe, d):
    before = time()
    def after(passthrough):
        interval = time() - before
        observe(interval)
        return passthrough
    d.addCallback(after)
    return d



_ROUNDTRIP_SUCCESS = Counter(
    "tahoe_lafs_roundtrip_benchmark_success_total",
    "Number of roundtrip benchmarks which completed successfully.",
)
_ROUNDTRIP_FAILURE = Counter(
    "tahoe_lafs_roundtrip_benchmark_failure_total",
    "Number of roundtrip benchmarks which failued somehow.",
)

_rate_buckets = tuple(n * 50 * 1024 for n in range(1, 20)) + (float("inf"),)

_LAST_READ = Gauge(
    "tahoe_lafs_roundtrip_benchmark_last_read_bytes_per_second",
    "Most recent measured transfer rate for reading a file on the grid.",
)
_LAST_WRITE = Gauge(
    "tahoe_lafs_roundtrip_benchmark_last_write_bytes_per_second",
    "Most recent measured transfer rate for writeing a file on the grid.",
)


from twisted.python.usage import (
    Options,
    UsageError,
)

class Options(Options):
    optParameters = [
        (u"introducer-furl", None, None, "The introducer fURL to use."),
        (u"mutable-file-cap", None, None, "An existing mutable file cap to re-write."),
        (u"client-directory", None, None, "The directory of an existing Tahoe-LAFS client to use.", FilePath),
    ]

    optFlags = [
        ("preserve-tahoe", None, "If a Tahoe-LAFS process is started, leave it running."),
    ]


@inlineCallbacks
def main(reactor, *argv):
    options = Options()
    try:
        options.parseOptions(argv)
    except UsageError as e:
        raise SystemExit(str(e))

    if options[u"client-directory"]:
        client = join_tahoe_lafs_client(options[u"client-directory"])
    else:
        introducer_furl = options[u"introducer-furl"]
        client = yield create_tahoe_lafs_client(
            reactor,
            introducer_furl=introducer_furl.decode("ascii"),
        )
        if options[u"preserve-tahoe"]:
            print(u"Tahoe: {}".format(client.tahoe_client.root.path))
        else:
            reactor.addSystemEventTrigger("before", "shutdown", lambda: client.shutdown())

    mutable_file_cap = options[u"mutable-file-cap"]

    if mutable_file_cap is None:
        mutable_file_cap = (yield client.create_mutable_file(b"xxx")).strip()
        print(mutable_file_cap)

    yield roundtrip_check(client, [mutable_file_cap.decode("ascii")])

    from prometheus_client import REGISTRY, write_to_textfile
    write_to_textfile("/tmp/roundtrip.prom", REGISTRY)



if __name__ == '__main__':
    from sys import argv
    from twisted.internet.task import react
    react(main, argv[1:])
