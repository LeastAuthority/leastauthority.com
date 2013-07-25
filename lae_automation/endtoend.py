
import time
from collections import namedtuple

from twisted.internet import defer
from twisted.application import service
from twisted.python.filepath import FilePath

from allmydata.util.pollmixin import PollMixin, TimeoutError
from allmydata.client import Client
from allmydata.monitor import Monitor
from allmydata.mutable.publish import MutableData

from lae_util import Namespace
from lae_util.timestamp import format_iso_time
from lae_automation.server import read_secrets_file, write_secrets_file, InvalidSecrets


class CheckFailed(Exception):
    pass


SecretsFile = namedtuple('SecretsFile', ['filepath', 'secrets'])


def check_server_end_to_end(secretsfp, secrets, stdout, stderr, recreate_test_file=False, timestamp=None):
    checker = EndToEndChecker()
    return checker.check(secretsfp, secrets, stdout, stderr, recreate_test_file, timestamp)


class EndToEndChecker(PollMixin):
    """This is a class only in order to use PollMixin."""

    def check(self, secretsfp, secrets, stdout, stderr, recreate_test_file=False, timestamp=None):
        if timestamp is None:
            timestamp = format_iso_time(time.time())

        publichost = secrets['publichost']

        client_dir = FilePath("monitoring_client")
        try:
            client_dir.makedirs()
        except OSError:
            if not client_dir.exists():
                raise

        # The logs and incidents are sometimes interesting, but we don't want them to cause a space leak,
        # so delete the *last* run's logs before each run.
        log_dir_path = client_dir.child('logs').path
        try:
            fileutil.rm_dir(log_dir_path)
        except OSError, e:
            print >>stderr, "Warning: couldn't remove log directory %r\n%s" % (log_dir_path, e)

        tahoe_cfg = ("[node]\n"
                     "nickname = LeastAuthority monitoring client\n"
                     "web.port = tcp:0:interface=127.0.0.1\n"   # make sure the client doesn't listen on an external interface
                     "[client]\n"
                     "introducer.furl = %s\n"
                     "[storage]\n"
                     "enabled = false\n" % (secrets['external_introducer_furl']))
        client_dir.child("tahoe.cfg").setContent(tahoe_cfg)

        # Run the client in-process.
        parent = service.MultiService()
        parent.startService()
        client = Client(client_dir.path)
        client.setServiceParent(parent)
        storage_broker = client.get_storage_broker()

        def _print(res, s):
            print >>stdout, s
            if res:
                print >>stdout, res
            return res

        # Wait until the client is connected to the introducer.
        d = self.poll(client.connected_to_introducer, pollinterval=0.2, timeout=10)
        def _connection_err(f):
            f.trap(TimeoutError)
            raise CheckFailed("Error for %s: could not connect to the introducer" % (publichost,))
        d.addErrback(_connection_err)

        # Check whether the client is connected to the right storage server.
        ns = Namespace()
        def _connected_to_server():
            ns._connected_servers = frozenset([s.get_longname() for s in storage_broker.get_connected_servers()])
            return len(ns._connected_servers) > 0
        d.addCallback(lambda ign: self.poll(_connected_to_server, pollinterval=0.5, timeout=10))

        def _check_connections(ign):
            server_nodeid = secrets['server_nodeid']
            if server_nodeid not in ns._connected_servers:
                raise CheckFailed("Error for %s: not connected to the expected server %r" % (publichost, server_nodeid))

            unexpected_servers = ns._connected_servers - frozenset([server_nodeid])
            if len(unexpected_servers) > 0:
                print >>stderr, "Warning for %s: connected to unexpected server(s) %r" % (publichost, unexpected_servers)
        d.addCallback(_check_connections)

        if 'test_uri' not in secrets or recreate_test_file:
            d.addCallback(_print, "Creating test file for %s..." % (publichost,))
            d.addCallback(lambda ign: client.create_mutable_file(contents="Monitoring at _"))
            def _created(node):
                secrets['test_uri'] = node.get_write_uri()
                write_secrets_file(secretsfp, secrets)
            d.addCallback(_created)
            def _create_err(f):
                raise CheckFailed("Error for %s: could not create test file: %s" % (publichost, f))
            d.addErrback(_create_err)

        def _do_test_write(ign):
            if 'test_uri' in secrets:
                node = client.create_node_from_uri(secrets['test_uri'])
                monitor = Monitor()

                d2 = defer.succeed(None)
                d2.addCallback(_print, "Checking test file for %s..." % (publichost,))
                d2.addCallback(lambda ign: node.check(monitor, verify=True))
                def _verified(res):
                    if not res.is_healthy:
                        print >>stderr, "Warning for %s: test file is unhealthy: %s" % (publichost, res.get_summary())
                def _verify_err(f):
                    print >>stderr, "Warning for %s: test file could not be verified: %s" % (publichost, f)
                d2.addCallbacks(_verified, _verify_err)

                d2.addCallback(lambda ign: node.download_best_version())
                def _check_content(content):
                    if not content.startswith("Monitoring at "):
                        raise CheckFailed("Error for %s: test file had unexpected content" % (publichost,))
                d2.addCallback(_check_content)

                d2.addCallback(lambda ign: node.overwrite(MutableData("Monitoring at %s" % (timestamp,))))
                def _overwrite_err(f):
                    raise CheckFailed("Error for %s: could not overwrite test file: %s" % (publichost, f))
                d2.addErrback(_overwrite_err)
                d2.addCallback(_print, "Success for %s!" % (publichost,))
                return d2
        d.addCallback(_do_test_write)

        def _stop_client(res):
            d2 = client.disownServiceParent()
            def _print_err(f):
                print >>stderr, "Warning for %s: exception shutting down client: %s" % (publichost, f)
            d2.addErrback(_print_err)
            d2.addCallback(lambda ign: res)
            return d2
        d.addBoth(_stop_client)
        return d


class DictOfLists(dict):
    def add(self, key, value):
        if key in self:
            self[key].append(value)
        else:
            self[key] = [value]

    def remove(self, key, value):
        if not key in self:
            return
        self[key].remove(value)
        if not self[key]:
            del self[key]


def read_secrets_dir(secretsdirfp, stdout, stderr):
    all_secrets_by_bucket = DictOfLists()
    all_secrets_by_host = DictOfLists()

    if not secretsdirfp.isdir():
        print >>stderr, "Error: Secrets directory not found at %r" % (secretsdirfp.path,)
    else:
        print >>stdout, "Reading secrets files..."
        secretsfps = secretsdirfp.children()

        # We use the bucket name to identify servers that were launched on behalf of the same customer.
        # It is normal for there to be secrets that do not correspond to a running server; these
        # should not cause warnings, because other checks will have warned that the server is not
        # running if it was expected to be.

        for secretsfp in secretsfps:
            if secretsfp.getsize() > 0:
                try:
                    host_secrets = read_secrets_file(secretsfp)
                except InvalidSecrets, e:
                    print >>stderr, str(e)
                else:
                    sf = SecretsFile(filepath=secretsfp, secrets=host_secrets)
                    all_secrets_by_bucket.add(host_secrets['bucket_name'], sf)
                    all_secrets_by_host.add(host_secrets['publichost'], sf)

    def _filename(sf): return sf.filepath.basename()

    def _sort_and_print_duplicates(all_secrets, key_description):
        secrets_by_key = {}
        for (key, candidate_set) in sorted(all_secrets.items()):
            # Sort the candidates by filename, which corresponds to the order of ISO 8601 UTC timestamps.
            candidates = sorted(candidate_set, key=_filename)
            secrets_by_key[key] = candidates[-1]

            if len(candidates) > 1:
                print >>stderr, "Multiple secrets files match %s %r:" % (key_description, key)
                for sf in candidates:
                    print >>stderr, "  %s" % (_filename(sf),)

        return secrets_by_key

    _sort_and_print_duplicates(all_secrets_by_bucket, 'bucket')
    return _sort_and_print_duplicates(all_secrets_by_host, 'host')
