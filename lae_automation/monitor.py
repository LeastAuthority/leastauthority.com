
import time, traceback
from cStringIO import StringIO
from ConfigParser import SafeConfigParser

from twisted.python.filepath import FilePath

from lae_automation.server import run, set_host_and_key, NotListeningError
from lae_automation.aws.queryapi import pubIPextractor
from lae_util.send_email import send_plain_email, FROM_EMAIL
from lae_util.servers import append_record
from lae_util.timestamp import parse_iso_time


# Anything printed to stderr counts as a notifiable problem.

def check_server(publichost, monitor_privkey_path, stdout, stderr):
    try:
        set_host_and_key(publichost, monitor_privkey_path, username="monitor")

        psout = run('ps -fC tahoe || true')
        pslines = psout.splitlines()
        if not pslines[0].startswith("UID"):
            print >>stderr, "Error: Host %s unexpected ps output %r.\n" % (publichost, psout)
            return False

        nodes = []
        for line in pslines[1:]:
            fields = line.split()
            [uid, pid, parent_pid, c, start_time, tty, proc_time] = fields[:7]
            cmd = fields[7:]
            if [True for c in cmd if c.startswith('@')]:
                # not a node
                continue
            if not (len(cmd) == 4 and cmd[0].endswith('/python') and cmd[1].endswith('/tahoe') and
                    cmd[2] in ('start', 'restart') and cmd[3] in ('introducer', 'storageserver')):
                print >>stderr, "Error: Host %s unexpected command %r." % (publichost, " ".join(cmd))
                return False
            nodes.append(cmd[3])

        nodes.sort()
        if nodes != ['introducer', 'storageserver']:
            print >>stderr, "Error: Host %s expected nodes are not running. Actual nodes are %r." % (publichost, nodes)
            return False

        tahoe_cfg = run('cat /home/customer/storageserver/tahoe.cfg')
        config = SafeConfigParser()
        config.readfp(StringIO(tahoe_cfg))
        s3host = config.get('storage', 's3.bucket') + '.s3.amazonaws.com'
        nslookup = run('nslookup ' + s3host)
        if not 'answer:' in nslookup:
            print >>stderr, "Error: Host %s was not able to resolve %r:\n%r" % (publichost, s3host, nslookup)
            return False

        return True
    except NotListeningError:
        print >>stderr, "Error: Host %s is not listening." % (publichost,)
        return False
    except (Exception, SystemExit):
        print >>stderr, "Exception while checking host %s:" % (publichost,)
        traceback.print_exc(file=stderr)
        return False


def check_servers(host_list, monitor_privkey_path, stdout, stderr):
    success = True
    for publichost in host_list:
        print >>stdout, "Checking %r..." % (publichost,)
        success = check_server(publichost, monitor_privkey_path, stdout, stderr) and success

    return success


def compare_servers_to_local(remotepropstuplelist, localstate, stdout, stderr, now=None):
    if now is None:
        now = time.time()
    running_list = []
    for (rpt_launch_time, rpt_instance_id, rpt_host, rpt_status) in remotepropstuplelist:
        rpt_publichost = pubIPextractor(rpt_host)
        rpt_publichost_s = rpt_publichost or '<no public IP>'

        if not localstate.has_key(rpt_instance_id):
            # unknown instance
            launch_time = parse_iso_time(rpt_launch_time)
            if now - launch_time < 10*60:
                print >>stdout, ("Note: Ignoring unknown %s instance %s at %s because it was launched less than 10 minutes ago at %s."
                                 % (rpt_status, rpt_instance_id, rpt_publichost_s, rpt_launch_time))
            elif rpt_status == 'terminated':
                print >>stdout, ("Note: Ignoring %s instance %s at %s launched at %s."
                                 % (rpt_status, rpt_instance_id, rpt_publichost_s, rpt_launch_time))
            else:
                print >>stderr, ("Warning: The %s instance %s at %s launched at %s is not in the list of known servers."
                                 % (rpt_status, rpt_instance_id, rpt_publichost_s, rpt_launch_time))

                if rpt_status == 'running' and rpt_publichost:
                    running_list.append(rpt_publichost)
        else:
            # known instance
            (launch_time, publichost, status) = localstate[rpt_instance_id]
            if status == 'ignore' or (status == 'not_running' and rpt_status != 'running'):
                print >>stdout, ("Note: Ignoring %s instance %s at %s launched at %s."
                                 % (rpt_status, rpt_instance_id, rpt_publichost_s, rpt_launch_time))
            else:
                if rpt_status == 'running' and rpt_publichost:
                    running_list.append(rpt_publichost)

                if publichost != rpt_publichost:
                    print >>stderr, ("Warning: The %s instance %s launched at %s changed public IP from %s to %s."
                                     % (rpt_status, rpt_instance_id, rpt_launch_time, publichost, rpt_publichost_s))

                if launch_time != rpt_launch_time:
                    print >>stderr, ("Warning: The %s instance %s at %s changed launch time from %s to %s (probably restarted)."
                                     % (rpt_status, rpt_instance_id, rpt_publichost_s, launch_time, rpt_launch_time))

                if status != rpt_status:
                    print >>stderr, ("Warning: The %s instance %s at %s launched at %s was expected to be %s."
                                     % (rpt_status, rpt_instance_id, rpt_publichost_s, rpt_launch_time, status))
            del localstate[rpt_instance_id]

    printed_heading = False
    for key in localstate:
        (s_launch_time, s_publichost, s_status) = localstate[key]
        if s_status not in ('ignore', 'not_running'):
            if not printed_heading:
                print >>stderr
                print >>stderr, "The following known servers were not found by the AWS query:"
                printed_heading = True

            print >>stderr, ("Instance ID: %s  Public IP: %-15s  Launch time: %s  Expected status: %s"
                             % (key, s_publichost, s_launch_time, s_status))

    if printed_heading:
        print >>stderr

    return running_list


# The format of each line is RECORD_ADDED_TIME,LAUNCH_TIME,INSTANCE_ID,PUBLICHOST[,STATUS]

def _parse_serverinfo_line(line):
    info = tuple(line.split(',')[1:])
    if len(info) == 4:
        return info
    else:
        return info + ('running',)

def read_serverinfo(pathtoserverinfo):
    serverinfofp = FilePath(pathtoserverinfo)
    listofinfostrings = serverinfofp.getContent().split('\n')
    listofinfotuples = [_parse_serverinfo_line(infostring) for infostring in listofinfostrings if infostring]
    return listofinfotuples


def write_serverinfo(pathtoserverinfo, remotepropstuplelist):
    serverinfofp = FilePath(pathtoserverinfo)
    serverinfofp.setContent("")
    for rpt in remotepropstuplelist:
        append_record(serverinfofp, rpt[0], rpt[1], pubIPextractor(rpt[2]))


MONITORING_EMAIL_SUBJECT = "Least Authority Enterprises monitoring report"

MONITORING_EMAIL_BODY_BROKEN = """Hello, monitoring script here.

The following problem(s) may need investigation:

%s

--\x20
multiservercheck.py
"""

MONITORING_EMAIL_BODY_WORKING = """Hello, monitoring script here.

Everything appears to be working again, as far as I can tell.

--\x20
multiservercheck.py
"""

FROM_ADDRESS = "Monitoring <%s>" % (FROM_EMAIL,)
TO_EMAIL = "info@leastauthority.com"
TO_EMAIL2 = "monitoring@leastauthority.com"


def send_monitoring_report(errors):
    if errors:
        content = MONITORING_EMAIL_BODY_BROKEN % (errors,)
    else:
        content = MONITORING_EMAIL_BODY_WORKING
    headers = {
               "From": FROM_ADDRESS,
               "Subject": MONITORING_EMAIL_SUBJECT,
              }

    d = send_plain_email(FROM_EMAIL, TO_EMAIL, content, headers)
    d.addBoth(lambda ign: send_plain_email(FROM_EMAIL, TO_EMAIL2, content, headers))
    return d
