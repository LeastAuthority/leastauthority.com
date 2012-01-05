
from lae_automation.server import run, set_host_and_key


def check_server(public_host, monitor_privkey_path, stdout, stderr):
    set_host_and_key(public_host, monitor_privkey_path, username="monitor")

    psout = run('ps -fC tahoe')
    pslines = psout.splitlines()
    if not pslines[0].startswith("UID"):
        stderr.write("%s: Unexpected ps output %r\n" % (public_host, psout))
        return False

    nodes = []
    for line in pslines[1:]:
        fields = line.split()
        [uid, pid, parent_pid, c, start_time, tty, time] = fields[:6]
        cmd = fields[6:]
        if not (len(cmd) == 4 and cmd[0].endswith('/python') and cmd[1].endswith('/tahoe') and
                cmd[2] in ('start', 'restart') and cmd[3] in ('introducer', 'storageserver')):
            stderr.write("%s: Unexpected command %s\n" % (public_host, " ".join(cmd)))
            return False
        nodes.append(cmd[3])

    nodes.sort()
    if nodes != ['introducer', 'storageserver']:
        stderr.write("%s: Expected nodes are not running. Actual nodes are %r" % (public_host, nodes))
        return False

    return True


def check_servers(host_list, monitor_privkey_path, stdout, stderr):
    # TODO: check that no expected servers are missing.
    success = True
    for (public_host, private_host) in host_list:
        print "Checking %r..." % (public_host,)
        success = check_server(public_host, monitor_privkey_path, stdout, stderr) and success

    return success
