# Copyright Least Authority Enterprises.
# See LICENSE for details.

from tempfile import mkstemp, mkdtemp
from os import fdopen
from json import dumps
from subprocess import check_call
from time import time

from hypothesis import given, settings

from testtools.matchers import (
    Equals, AllMatch, MatchesListwise,
    Not,
    LessThan,
    AfterPreprocessing,
)

from fixtures import Fixture

from twisted.python.filepath import FilePath

from foolscap.furl import decode_furl, encode_furl

from .testcase import TestBase
from .matchers import hasContents, hasContentsMatching, hasConfiguration
from .strategies import introducer_configuration, storage_configuration

from lae_automation.server import marshal_tahoe_configuration

CONFIGURE_TAHOE = FilePath(__file__).parent().parent().child(b"configure-tahoe")

# Parent settings for tests that don't need to discover a huge number
# of successful examples to be considered a success.
simple = settings(max_examples=5)

def configure_tahoe(configuration, root):
    fd, name = mkstemp()
    with fdopen(fd, "w+") as fObj:
        fObj.write(dumps(configuration))
        fObj.seek(0)
        # Assume it is executable and has a sane interpreter.
        check_call([CONFIGURE_TAHOE.path, root], stdin=fObj)


class TahoeNodes(Fixture):
    def __init__(self, root):
        Fixture.__init__(self)
        self.root = root

    def _setUp(self):
        self.nodes = FilePath(self.root)
        self.introducer = self.nodes.child(b"introducer")
        self.introducer.makedirs()
        self.storage = self.nodes.child(b"storage")
        self.storage.makedirs()


class ConfigureTahoeTests(TestBase):
    """
    Tests for the command-line ``configure-tahoe`` tool.
    """
    def setUp(self):
        TestBase.setUp(self)
        self.nodes = self.useFixture(TahoeNodes(mkdtemp()))

    @given(introducer_configuration(), storage_configuration())
    @settings(parent=simple)
    def test_gatherers_missing(self, introducer_config, storage_config):
        """
        If the log and stats gatherers are not given, the storage and
        introducer configurations are written with empty strings for
        these fields.
        """
        config = marshal_tahoe_configuration(
            introducer_pem=introducer_config["node_pem"],
            storage_pem=storage_config["node_pem"],
            storage_privkey=storage_config["node_privkey"],
            introducer_port=introducer_config["port"],
            storageserver_port=storage_config["port"],
            bucket_name=storage_config["bucket_name"],
            key_prefix=storage_config["key_prefix"],
            publichost=storage_config["publichost"],
            privatehost=storage_config["privatehost"],
            introducer_furl=storage_config["introducer_furl"],
            s3_access_key_id=storage_config["s3_access_key_id"],
            s3_secret_key=storage_config["s3_secret_key"],
            log_gatherer_furl=None,
            stats_gatherer_furl=None,
        )
        configure_tahoe({"introducer": config["introducer"]}, self.nodes.introducer.path)
        configure_tahoe({"storage": config["storage"]}, self.nodes.storage.path)

        config_files = [
            self.nodes.introducer.child(b"tahoe.cfg"),
            self.nodes.storage.child(b"tahoe.cfg"),
        ]
        self.assertThat(config_files, AllMatch(
            hasConfiguration({
                ("node", "log_gatherer.furl", ""),
                ("client", "stats_gatherer.furl", ""),
            })
        ))

    @given(introducer_configuration(), storage_configuration())
    @settings(parent=simple)
    def test_complete(self, introducer_config, storage_config):
        """
        Introducer and storage configuration can be supplied via ``configure_tahoe``.
        """
        introducer_furl = introducer_config["introducer_furl"]

        config = marshal_tahoe_configuration(
            introducer_pem=introducer_config["node_pem"],
            storage_pem=storage_config["node_pem"],
            storage_privkey=storage_config["node_privkey"],
            introducer_port=introducer_config["port"],
            storageserver_port=storage_config["port"],
            bucket_name=storage_config["bucket_name"],
            key_prefix=storage_config["key_prefix"],
            publichost=storage_config["publichost"],
            privatehost=storage_config["privatehost"],
            introducer_furl=introducer_furl,
            s3_access_key_id=storage_config["s3_access_key_id"],
            s3_secret_key=storage_config["s3_secret_key"],
            log_gatherer_furl=introducer_config["log_gatherer_furl"],
            stats_gatherer_furl=introducer_config["stats_gatherer_furl"],
        )
        configure_tahoe({"introducer": config["introducer"]}, self.nodes.introducer.path)
        configure_tahoe({"storage": config["storage"]}, self.nodes.storage.path)

        intro_config_path = self.nodes.introducer.child(b"tahoe.cfg")
        storage_config_path = self.nodes.storage.child(b"tahoe.cfg")
        config_files = [intro_config_path, storage_config_path]

        # If the log and stats gatherers are given, the storage and
        # introducer configurations are written with those values for
        # those fields.
        self.expectThat(
            config_files, AllMatch(
            hasConfiguration({
                ("node", "log_gatherer.furl", introducer_config["log_gatherer_furl"]),
                ("client", "stats_gatherer.furl", introducer_config["stats_gatherer_furl"]),
            })
        ))

        # The introducer furl in the introducer configuration is
        # written to the ``private/introducer.furl`` file in the
        # introducer's state/configuration directory and to the
        # storage node's configuration file.
        self.expectThat(
            self.nodes.introducer.descendant([b"private", b"introducer.furl"]),
            hasContents(introducer_furl),
        )
        tub_id, location_hints, name = decode_furl(introducer_furl)
        port = location_hints[0].split(":")[1]
        location_hints[:0] = [storage_config["privatehost"] + ":" + port]
        internal_introducer_furl = encode_furl(tub_id, location_hints, name)
        self.expectThat(
            storage_config_path,
            hasConfiguration({
                ("client", "introducer.furl", internal_introducer_furl),
            }),
        )
        self.expectThat(
            self.nodes.storage.child(b"announcement-seqnum"),
            hasContentsMatching(
                # The second hand could click over between when the file is
                # written and when this test code runs.  In fact, it could
                # click over multiple times... But the only way to really fix
                # that is to parameterize the clock and the structure of
                # configure_tahoe makes that tricky.  So just suppose that one
                # second is all the leeway we need to make this reliable.
                AfterPreprocessing(int, Not(LessThan(int(time() - 1)))),
            ),
        )


class MarshalTahoeConfiguration(TestBase):
    """
    Tests for ``marshal_tahoe_configuration``.
    """
    def test_structure_matches_strategy(self):
        """
        The basic structure of the result returned matches the structure
        created by the hypothesis strategies used by other tests.
        """
        marshalled = marshal_tahoe_configuration(
            "", "", "", 0, 0, "", "", "", "", "", "", "",
        )
        introducer = introducer_configuration().example()
        storage = storage_configuration().example()

        self.assertThat(
            [set(introducer), set(storage)],
            MatchesListwise([
                Equals(set(marshalled["introducer"])),
                Equals(set(marshalled["storage"])),
            ]),
        )
