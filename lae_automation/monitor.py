
import time, traceback
from twisted.python.filepath import FilePath

from lae_automation.server import run, set_host_and_key
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
            if not (len(cmd) == 4 and cmd[0].endswith('/python') and cmd[1].endswith('/tahoe') and
                    cmd[2] in ('start', 'restart') and cmd[3] in ('introducer', 'storageserver')):
                print >>stderr, "Error: Host %s unexpected command %r." % (publichost, " ".join(cmd))
                return False
            nodes.append(cmd[3])

        nodes.sort()
        if nodes != ['introducer', 'storageserver']:
            print >>stderr, "Error: Host %s expected nodes are not running. Actual nodes are %r." % (publichost, nodes)
            return False

        return True
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
    host_list = []
    for rpt in remotepropstuplelist:
        publichost = pubIPextractor(rpt[2])
        if not publichost:
            print >>stderr, ("Warning: Host launched at %s with instance ID %s has no public IP (maybe it has been terminated)."
                             % (rpt[0], rpt[1]))
        elif not localstate.has_key(publichost):
            launch_time = parse_iso_time(rpt[0])
            if now - launch_time < 10*60:
                print >>stdout, "Note: Ignoring host %s because it was launched less than 10 minutes ago at %s." % (publichost, rpt[0])
            else:
                print >>stderr, "Warning: Host %s is not in the list of known servers." % (publichost,)
                host_list.append(publichost)
        else:
            host_list.append(publichost)
            if localstate[publichost][0] != rpt[0]:
                print >>stderr, ("Warning: Host %s launch time changed from %s to %s (probably rebooted)."
                                 % (publichost, localstate[publichost][0], rpt[0]))
            if localstate[publichost][1] != rpt[1]:
                print >>stderr, ("Warning: Host %s changed instance ID from %s to %s."
                                 % (publichost, localstate[publichost][1], rpt[1]))
            del localstate[publichost]

    if localstate:
        print >>stderr, "The following known servers were not found by the AWS query:"
        for key in localstate:
            s = localstate[key]
            print >>stderr, "Host %s Launch time: %s Instance ID: %s" % (key, s[0], s[1])

    return host_list


# The format of each line is RECORD_ADDED_TIME,LAUNCH_TIME,INSTANCE_ID,PUBLICHOST

def read_serverinfo(pathtoserverinfo):
    listofinfostrings = FilePath(pathtoserverinfo).getContent().split('\n')
    listofinfotuples = [infostring.split(',')[1:] for infostring in listofinfostrings if infostring]
    return listofinfotuples


def write_serverinfo(pathtoserverinfo, remotepropstuplelist):
    open(pathtoserverinfo, "w").close()
    for rpt in remotepropstuplelist:
        append_record(pathtoserverinfo, rpt[0], rpt[1], pubIPextractor(rpt[2]))


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
