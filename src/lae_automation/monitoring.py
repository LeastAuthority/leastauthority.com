from __future__ import unicode_literals, print_function

from functools import partial
from itertools import count
from random import randrange
from tempfile import mktemp
from sys import stdout

from twisted.internet.defer import Deferred
from twisted.internet.task import deferLater, react
from twisted.application.service import MultiService
from twisted.python.filepath import FilePath
from twisted.logger import (
    globalLogBeginner,
    textFileLogObserver,
)

from foolscap.api import Tub
from foolscap.logging.log import bridgeLogsToTwisted

import allmydata
from allmydata.interfaces import HASH_SIZE
from allmydata.introducer.client import IntroducerClient
from allmydata.storage_client import NativeStorageServer


SECRETS = (
    # write enabler
    b"w" * HASH_SIZE,
    # renew secret
    b"r" * HASH_SIZE,
    # cancel secret
    b"c" * HASH_SIZE,
)


class _Sequencer(object):
    def __init__(self):
        self._count = count()

    def __call__(self):
        return next(self._count), randrange(2 ** 32)


def main(reactor, furl):
    # bridgeLogsToTwisted(
    #     filter=lambda event: (
    #         not event.get("facility", "").startswith("foolscap")
    #     ),
    # )
    globalLogBeginner.beginLoggingTo([
        textFileLogObserver(stdout),
    ])
    parent = MultiService()

    tub = Tub()
    tub.setServiceParent(parent)
    tub.listenOn(b"tcp:12345")
    tub.setLocation(b"127.0.0.1:12345")
    print("Flogging to", tub.getLogPortFURL())

    introducer_cache = FilePath(mktemp())
    introducer = IntroducerClient(
        tub, furl,
        "monitoring",
        str(allmydata.__full_version__),
        "1.0.0",
        {},
        _Sequencer(),
        introducer_cache,
    )
    introducer.setServiceParent(parent)

    print("Subscribing to storage announcements...")
    done = Deferred()
    introducer.subscribe_to(b"storage", partial(got_storage, reactor, done, parent))

    parent.startService()
    done.addBoth(print)
    return done


def got_storage(reactor, done, parent, key_s, announcement):
    def create_tub(ignored):
        return Tub()

    storage = NativeStorageServer(
        b"foo",
        announcement,
        create_tub,
        {},
    )
    storage.setServiceParent(parent)
    print("Connecting to storage server...")
    storage.start_connecting(partial(triggered, reactor, done, storage))


def triggered(reactor, done, storage):
    print("Retrieving storage reference...")
    poll_for_connection(reactor, storage).chainDeferred(done)


def poll_for_connection(reactor, storage):
    if storage.is_connected():
        return connected(storage)
    print("...not connected...")
    return deferLater(reactor, 3, poll_for_connection, reactor, storage)


def connected(storage):
    print("Connected.")
    return maybe_initialize_index(storage, b"deadbeefdeadbeef")


def maybe_initialize_index(storage, storage_index):
    print("Getting buckets for probe index...")

    # Get a remote reference to the allmydata.storage.server.StorageServer.
    rref = storage.get_rref()

    probe_content = b"Hello world."

    index = b"deadbeefdeadbeef"

    print("Checking existing probe content...")
    d = check_if_exists(rref, index)
    def checked(exists):
        if exists:
            print("Replacing existing probe content...")
            return rewrite_content(rref, index, probe_content)
        else:
            print("Writing initial probe content...")
            return create_content(rref, index, probe_content)
    d.addCallback(checked)
    def recheck(ignored):
        print("Re-checking probe content...")
        return check_content(rref, index, probe_content)
    d.addCallback(recheck)
    return d


def check_content(rref, index, content):
    testv = (
        # Offset
        0,
        # Length
        len(content),
        # Operator
        b"eq",
        # Specimen
        content,
    )
    d = rref.callRemote(
        b"slot_testv_and_readv_and_writev",
        index,
        SECRETS,
        {0: ([testv], [], None)},
        [],
    )
    def check_testv(result):
        testv_is_good, ignored = result
        if testv_is_good:
            return "Probe checks out."
        raise Exception("Probe content mis-matches expectations.")
    d.addCallback(check_testv)
    return d


def check_if_exists(rref, index):
    testv = (
        # Offset
        0,
        # Length
        1,
        # Operator
        b"ne",
        # Specimen
        b"",
    )
    d = rref.callRemote(
        b"slot_testv_and_readv_and_writev",
        index,
        SECRETS,
        {0: ([testv], [], None)},
        [],
    )
    def check_testv(result):
        testv_is_good, ignored = result
        return testv_is_good
    d.addCallback(check_testv)
    return d


def rewrite_content(rref, index, content):
    writev = (
        # Offset
        0,
        # Data
        content,
    )
    d = rref.callRemote(
        b"slot_testv_and_readv_and_writev",
        index,
        SECRETS,
        {0: ([], [writev], None)},
        [],
    )
    d.addCallback(lambda ignored: None)
    return d


def create_content(rref, index, content):
    testv = (
        # Offset
        0,
        # Length
        1,
        # Operator
        b"eq",
        # Specimen
        b"",
    )
    writev = (
        # Offset
        0,
        # Data
        content,
    )

    d = rref.callRemote(
        b"slot_testv_and_readv_and_writev",
        index,
        SECRETS,
        {0: ([testv], [writev], None)},
        [],
    )
    d.addCallback(lambda ignored: None)
    return d

from sys import argv
react(main, argv[1:])
