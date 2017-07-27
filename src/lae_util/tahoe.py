# Copyright Least Authority Enterprises.
# See LICENSE for details.

"""
Utilities for managing a Tahoe-LAFS client node and interacting with its
web API.
"""

from os import environ
from time import time
from tempfile import mkdtemp
from random import randrange

from twisted.python.filepath import FilePath
from twisted.internet.defer import succeed
from twisted.internet.interfaces import IProcessTransport
from twisted.internet.protocol import ProcessProtocol
from twisted.internet.utils import getProcessOutput
from twisted.internet.task import deferLater

import treq as _treq

import attr
from attr.validators import optional, instance_of, provides

from hyperlink import URL

from prometheus_client import Summary, Counter

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
    configuration = attr.ib(validator=instance_of(_TahoeConfiguration))
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



@attr.s(frozen=True)
class _LAFS(object):
    tahoe_client = attr.ib(validator=instance_of(_TahoeClient))
    tahoe_process = attr.ib(validator=instance_of(_TahoeProcess))
    treq = attr.ib()

    def shutdown(self):
        return self.tahoe_process.stop()


    @property
    def _root_uri(self):
        return self.tahoe_client.root_uri


    def _put_file(self, uri, contents):
        d = self.treq.put(uri.to_uri().to_text().encode("ascii"), contents)
        d.addCallback(self.treq.text_content)
        return d


    def create_mutable_file(self, contents, format=u"SDMF"):
        uri = self._root_uri.child(u"uri").add(u"format", format)
        return self._put_file(uri, contents)


    def write_mutable_file(self, loc, contents):
        uri = self._root_uri.child(u"uri", *loc)
        return self._put_file(uri, contents)


    def read_file(self, loc):
        uri = self._root_uri.child(u"uri", *loc)
        d = self.treq.get(uri.to_uri().to_text().encode("ascii"))
        d.addCallback(self.treq.content)
        return d



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


def roundtrip_check(lafs, mutable_file_loc):
    pattern = u"{:04x}".format(randrange(2 ** 16)).encode("ascii")
    contents = 512 * (256 * pattern)
    size = len(contents)
    d = _measure_async_time(
        lambda interval: _WRITE_TIME.observe(size / interval),
        lafs.write_mutable_file(mutable_file_loc, contents),
    )
    def wrote(ignored):
        d = _measure_async_time(
            lambda interval: _READ_TIME.observe(size / interval),
            lafs.read_file(mutable_file_loc),
        )
        return d
    d.addCallback(wrote)

    def read(result):
        if contents == result:
            _ROUNDTRIP_SUCCESS.inc()
        else:
            raise Exception("mismatch response bytes:\n{}".format(result))
    d.addCallback(read)
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
_READ_TIME = Summary(
    "tahoe_lafs_roundtrip_benchmark_read_bytes_per_second",
    "Transfer rate for reading a file on the grid.",
)
_WRITE_TIME = Summary(
    "tahoe_lafs_roundtrip_benchmark_write_bytes_per_second",
    "Transfer rate for writing a file on the grid.",
)


def main(reactor):
    d = create_tahoe_lafs_client(
        reactor,
        introducer_furl=u"pb://gnykjul7ocigeptkecenytywj2jerivp@on2wexzzmvnhu22tpbixm3tbnvsgg.introducer.leastauthority.com:10000/jvl5jssmejcl3lboep7nqnooia4bsykf",
    )
    d.addCallback(
        roundtrip_check,
        [u"URI:DIR2:3m7fosu5lmgwrqvwrca3jgoyia:musow6lqrwnls35mupm6th7hfc2qrga2d4vtyq4knq6ewbtt2fca",
         u"scratch",
         u"random",
        ],
    )
    def done(ignored):
        from prometheus_client import REGISTRY, write_to_textfile
        write_to_textfile("/tmp/roundtrip.prom", REGISTRY)
    d.addCallback(done)
    return d



if __name__ == '__main__':
    from twisted.internet.task import react
    react(main, [])
