from lae_automation.server import run, set_host_and_key
from lae_automation.aws.queryapi import pubIPextractor
from twisted.python.filepath import FilePath

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
        [uid, pid, parent_pid, c, start_time, tty, time] = fields[:7]
        cmd = fields[7:]
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
        print >>stdout, "Checking %r..." % (public_host,)
        success = check_server(public_host, monitor_privkey_path, stdout, stderr) and success

    return success

def comparetolocal(remotepropstuplelist, localstate):
    host_list = []
    for rpt in remotepropstuplelist:
        pubIP = pubIPextractor(rpt[2])
        host_list.append( (pubIP, rpt[3]) )
        if not localstate.has_key(pubIP):
            print "Whoa nelly, there's an EC2 online that's not in the info list!!!"
            raise Exception("Something is very wrong, public IP %s is not in the list of known servers!" % pubIP)
        else:
            assert localstate[pubIP] == (rpt[0], rpt[1]), 'Expected_Launch: %s\tObserved_Launch: %s\nExpected_instanceID: %s\tObserved_instanceID: %s' % (localstate[pubIP][0], rpt[0], localstate[pubIP][1], rpt[1])
            localstate.pop(pubIP)
    if len(localstate.keys()) != 0:
        print "The following instances are listed as known servers, but did respond to the AWS query:"
        for key in localstate.keys():
            print "Public IP: %s InstanceID: %s Launchtime: %s" % (key, localstate[key][1], localstate[key][0])
    return host_list

def readserverinfocsv(pathtoserverinfo):
    listofinfostrings = FilePath(pathtoserverinfo).getContent().split('\n')
    listofinfotuples = [infostring.split(',') for infostring in listofinfostrings]
    return listofinfotuples
