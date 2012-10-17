
import time, traceback
from cStringIO import StringIO
from ConfigParser import SafeConfigParser

from twisted.python.filepath import FilePath

from lae_automation.server import run, set_host_and_key, NotListeningError
from lae_automation.aws.queryapi import pubIPextractor
from lae_util.send_email import send_plain_email
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
        publichost = pubIPextractor(rpt_host)
        if not publichost:
            print >>stderr, ("Warning: Host launched at %s with instance ID %s (which is %s) has no public IP. Maybe it has been terminated."
                             % (rpt_launch_time, rpt_instance_id, rpt_status))
        elif not localstate.has_key(publichost):
            launch_time = parse_iso_time(rpt_launch_time)
            if now - launch_time < 10*60:
                print >>stdout, ("Note: Ignoring host %s (which is %s) because it was launched less than 10 minutes ago at %s."
                                 % (publichost, rpt_status, rpt_launch_time))
            else:
                print >>stderr, ("Warning: Host %s (which is %s) is not in the list of known servers."
                                 % (publichost, rpt_status))
                running_list.append(publichost)
        else:
            (launch_time, instance_id, status) = localstate[publichost]
            if status == 'running':
                running_list.append(publichost)

            if launch_time != rpt_launch_time:
                print >>stderr, ("Warning: Host %s launch time changed from %s to %s (probably rebooted)."
                                 % (publichost, launch_time, rpt_launch_time))
            if instance_id != rpt_instance_id:
                print >>stderr, ("Warning: Host %s changed instance ID from %s to %s."
                                 % (publichost, instance_id, rpt_instance_id))
            if status != rpt_status:
                print >>stderr, ("Warning: Host %s status is %s when expected to be %s."
                                 % (publichost, rpt_status, status))
            del localstate[publichost]

    if localstate:
        print >>stderr, "The following known servers were not found by the AWS query:"
        for key in localstate:
            (s_launch_time, s_instance_id, s_status) = localstate[key]
            print >>stderr, ("Host %-15s  Launch time: %s  Instance ID: %s  Expected status: %s"
                             % (key, s_launch_time, s_instance_id, s_status))
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

SENDER_DOMAIN = "leastauthority.com"
FROM_EMAIL = "info@leastauthority.com"
FROM_ADDRESS = "Monitoring <%s>" % (FROM_EMAIL,)
TO_EMAIL = "info@leastauthority.com"
TO_EMAIL2 = "monitoring@leastauthority.com"
USER_AGENT = "Least Authority Enterprises e-mail sender"

SMTP_HOST = "smtp.googlemail.com"
SMTP_HOST2 = "127.0.0.1"
SMTP_PORT = 25
SMTP_USERNAME = FROM_EMAIL
SMTP_USERNAME2 = ''


def send_monitoring_report(errors, password_path='../smtppassword'):
    password = FilePath(password_path).getContent().strip()

    if errors:
        content = MONITORING_EMAIL_BODY_BROKEN % (errors,)
    else:
        content = MONITORING_EMAIL_BODY_WORKING
    headers = {
               "From": FROM_ADDRESS,
               "Subject": MONITORING_EMAIL_SUBJECT,
               "User-Agent": USER_AGENT,
               "Content-Type": 'text/plain; charset="utf-8"',
              }

    d = send_plain_email(SMTP_HOST, SMTP_USERNAME, password, FROM_EMAIL, TO_EMAIL,
                         content, headers, SENDER_DOMAIN, SMTP_PORT)
    d.addBoth(lambda ign: send_plain_email(SMTP_HOST2, SMTP_USERNAME2, '', FROM_EMAIL, TO_EMAIL2,
                                           content, headers, SENDER_DOMAIN, SMTP_PORT,
                                           requireSSL=False))
    return d
