from sys import executable
from tempfile import mkstemp, mkdtemp
from os import fdopen
from json import dumps
from subprocess import check_call

from hypothesis import given, settings

from testtools.matchers import Equals, AllMatch, MatchesListwise

from fixtures import Fixture

from twisted.python.filepath import FilePath

from .testcase import TestBase
from .matchers import hasContents, hasConfiguration
from .strategies import introducer_configuration, storage_configuration

from lae_automation.server import marshal_tahoe_configuration

CONFIGURE_TAHOE = FilePath(__file__).parent().parent().child(b"configure-tahoe")

# Parent settings for tests that don't need to discover a huge number
# of successful examples to be considered a success.
simple = settings(max_examples=5)

def configure_tahoe(configuration):
    fd, name = mkstemp()
    with fdopen(fd, "w+") as fObj:
        fObj.write(dumps(configuration))
        fObj.seek(0)
        # Launch it with sys.executable so it gets the same Python
        # environment, if we are in a virtualenv, as the test process.
        check_call([executable, CONFIGURE_TAHOE.path], stdin=fObj)


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
            bucket_name=storage_config["bucket_name"],
            publichost=storage_config["publichost"],
            privatehost=storage_config["privatehost"],
            introducer_furl=storage_config["introducer_furl"],
            s3_access_key_id=storage_config["s3_access_key_id"],
            s3_secret_key=storage_config["s3_secret_key"],
            log_gatherer_furl=None,
            stats_gatherer_furl=None,
        )
        config["introducer"]["root"] = self.nodes.introducer.path
        config["storage"]["root"] = self.nodes.storage.path
        configure_tahoe(config)

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
            bucket_name=storage_config["bucket_name"],
            publichost=storage_config["publichost"],
            privatehost=storage_config["privatehost"],
            introducer_furl=introducer_furl,
            s3_access_key_id=storage_config["s3_access_key_id"],
            s3_secret_key=storage_config["s3_secret_key"],
            log_gatherer_furl=introducer_config["log_gatherer_furl"],
            stats_gatherer_furl=introducer_config["stats_gatherer_furl"],
        )
        config["introducer"]["root"] = self.nodes.introducer.path
        config["storage"]["root"] = self.nodes.storage.path
        configure_tahoe(config)

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
        self.expectThat(
            storage_config_path,
            hasConfiguration({
                ("client", "introducer.furl", introducer_furl),
            }),
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
        marshalled = marshal_tahoe_configuration("", "", "", "", "", "", "", "", "")
        introducer = introducer_configuration().example()
        storage = storage_configuration().example()

        self.assertThat(
            [set(introducer), set(storage)],
            MatchesListwise([
                Equals(set(marshalled["introducer"])),
                Equals(set(marshalled["storage"])),
            ]),
        )
