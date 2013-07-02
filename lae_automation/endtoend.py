
import time, simplejson

from twisted.application import service
from twisted.python.filepath import FilePath

from allmydata.util.pollmixin import PollMixin, TimeoutError
from allmydata.client import Client
from allmydata.monitor import Monitor
from allmydata.mutable.publish import MutableData

from lae_util.timestamp import format_iso_time


class CheckFailed(Exception):
    pass


class Namespace:
    pass


def check_end_to_end(secretsfile, stderr, create_test_uri=False):
    checker = EndToEndChecker()
    return checker.check(secretsfile, stderr, create_test_uri)


class EndToEndChecker(PollMixin):
    def __init__(self):
        pass

    def check(self, secretsfile, stderr, create_test_uri=False):
        timestamp = format_iso_time(time.time())

        try:
            secrets_json = secretsfile.getContent()
        except Exception, e:
            raise CheckFailed("Error for %r: could not read secrets file: %s" % (secretsfile.path, e))

        try:
            secrets = simplejson.loads(secrets_json)
        except Exception, e:
            # Don't include the message of e since we can't be sure it doesn't contain secrets.
            # The exception class name should be safe.
            raise CheckFailed("Error for %r: could not parse secrets file: %s" % (secretsfile.path, e.__class__.__name__))

        d = self._do_check(secretsfile, secrets, stderr, timestamp, create_test_uri)
        return d

    def _do_check(self, secretsfile, secrets, stderr, timestamp, create_test_uri):
        if 'publichost' not in secrets:
            raise CheckFailed("Error for %r: no publichost available" % (secretsfile.path))
        publichost = secrets['publichost']

        if 'external_introducer_furl' not in secrets:
            raise CheckFailed("Error for %s: no introducer FURL available" % (publichost,))
        external_introducer_furl = secrets['external_introducer_furl']

        if 'server_nodeid' not in secrets:
            raise CheckFailed("Error for %s: no server node ID available" % (publichost,))
        server_nodeid = secrets['server_nodeid']

        if 'test_uri' not in secrets and not create_test_uri:
            print >>stderr, "Warning for %s: no test URI available" % (publichost,)

        client_dir = FilePath("monitoring_client")
        try:
            client_dir.makedirs()
        except OSError:
            if not client_dir.exists():
                raise

        tahoe_cfg = ("[node]\n"
                     "nickname = LeastAuthority monitoring client\n"
                     "web.port = tcp:0:interface=127.0.0.1\n"   # make sure the client doesn't listen on an external interface
                     "[client]\n"
                     "introducer.furl = %s\n"
                     "[storage]\n"
                     "enabled = false\n" % (external_introducer_furl))
        client_dir.child("tahoe.cfg").setContent(tahoe_cfg)

        # Run the client in-process.
        parent = service.MultiService()
        parent.startService()
        client = Client(client_dir.path)
        client.setServiceParent(parent)
        storage_broker = client.get_storage_broker()

        def _print(res, s):
            print >>stderr, s
            if res:
                print >>stderr, res
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
            if server_nodeid not in ns._connected_servers:
                raise CheckFailed("Error for %s: not connected to the expected server %r" % (publichost, server_nodeid))

            unexpected_servers = ns._connected_servers - frozenset([server_nodeid])
            if len(unexpected_servers) > 0:
                print >>stderr, "Warning for %s: connected to unexpected server(s) %r" % (publichost, unexpected_servers)
        d.addCallback(_check_connections)

        if create_test_uri:
            d.addCallback(lambda ign: client.create_mutable_file(contents="Monitoring at _"))
            def _created(node):
                secrets['test_uri'] = node.get_write_uri()
                self._write_secrets(secretsfile, secrets)
            d.addCallback(_created)
            def _create_err(f):
                raise CheckFailed("Error for %s: could not create test file: %s" % (publichost, f))
            d.addErrback(_create_err)

        def _do_test_write(ign):
            # We already warned about the case of (no test URI and not create_test_uri) above.
            if 'test_uri' in secrets:
                node = client.create_node_from_uri(secrets['test_uri'])
                monitor = Monitor()

                d2 = node.check(monitor, verify=True)
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

    def _write_secrets(self, secretsfile, secrets):
        if 'test_uri' not in secrets:
            raise AssertionError("test_uri not set for %r" % (secretsfile.path,))
        secrets_json = simplejson.dumps(secrets) + "\n"
        secretsfile.setContent(secrets_json)
