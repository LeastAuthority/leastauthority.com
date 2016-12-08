from sys import executable
from tempfile import mkstemp, mkdtemp
from os import fdopen
from json import dumps
from subprocess import check_call
from ConfigParser import SafeConfigParser

from hypothesis import given, settings

from testtools import TestCase
from testtools.matchers import AfterPreprocessing, Equals, AllMatch

from fixtures import Fixture

from twisted.python.filepath import FilePath

from .strategies import introducer_configuration, storage_configuration

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


def hasConfiguration(fields):
    expected_fields = sorted(fields)

    def _readConfig(config_path):
        config = SafeConfigParser()
        config.readfp(config_path.open())
        actual_fields = sorted(
            (section, field, config.get(section, field))
            for (section, field, _)
            in expected_fields
        )
        return actual_fields
    return AfterPreprocessing(_readConfig, Equals(expected_fields))


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


class ConfigureTahoeTests(TestCase):
    """
    Tests for the command-line ``configure-tahoe`` tool.
    """
    def setUp(self):
        TestCase.setUp(self)
        self.nodes = self.useFixture(TahoeNodes(mkdtemp()))

    @given(introducer_configuration(), storage_configuration())
    @settings(parent=simple)
    def test_gatherers_missing(self, introducer_config, storage_config):
        """
        If the log and stats gatherers are not given, the storage and
        introducer configurations are written with empty strings for
        these fields.
        """
        introducer_config["root"] = self.nodes.introducer.path
        storage_config["root"] = self.nodes.storage.path

        for config in [introducer_config, storage_config]:
            for gatherer in {"log", "stats"}:
                config.pop(gatherer + "_gatherer_furl", None)


        config = dict(introducer=introducer_config, storage=storage_config)
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
    def test_gatherers(self, introducer_config, storage_config):
        """
        If the log and stats gatherers are given, the storage and
        introducer configurations are written with those values for
        those fields.
        """
        introducer_config["root"] = self.nodes.introducer.path
        storage_config["root"] = self.nodes.storage.path

        # They should match anyway and this makes the assertion below
        # easier.  Also, one is just as randomly generated as the
        # other...
        introducer_config["log_gatherer_furl"] = storage_config["log_gatherer_furl"]
        introducer_config["stats_gatherer_furl"] = storage_config["stats_gatherer_furl"]

        config = dict(introducer=introducer_config, storage=storage_config)
        configure_tahoe(config)

        config_files = [
            self.nodes.introducer.child(b"tahoe.cfg"),
            self.nodes.storage.child(b"tahoe.cfg"),
        ]
        self.assertThat(
            config_files, AllMatch(
            hasConfiguration({
                ("node", "log_gatherer.furl", introducer_config["log_gatherer_furl"]),
                ("client", "stats_gatherer.furl", introducer_config["stats_gatherer_furl"]),
            })
        ))
