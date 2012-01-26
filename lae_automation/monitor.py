
import traceback
from twisted.python.filepath import FilePath

from lae_automation.server import run, set_host_and_key
from lae_automation.aws.queryapi import pubIPextractor
from lae_util.send_email import send_plain_email
from lae_util.servers import append_record


# Anything printed to stderr counts as a notifiable problem.

def check_server(public_host, monitor_privkey_path, stdout, stderr):
    try:
        set_host_and_key(public_host, monitor_privkey_path, username="monitor")

        psout = run('ps -fC tahoe || true')
        pslines = psout.splitlines()
        if not pslines[0].startswith("UID"):
            print >>stderr, "Error: Host %s unexpected ps output %r.\n" % (public_host, psout)
            return False

        nodes = []
        for line in pslines[1:]:
            fields = line.split()
            [uid, pid, parent_pid, c, start_time, tty, time] = fields[:7]
            cmd = fields[7:]
            if not (len(cmd) == 4 and cmd[0].endswith('/python') and cmd[1].endswith('/tahoe') and
                    cmd[2] in ('start', 'restart') and cmd[3] in ('introducer', 'storageserver')):
                print >>stderr, "Error: Host %s unexpected command %r." % (public_host, " ".join(cmd))
                return False
            nodes.append(cmd[3])

        nodes.sort()
        if nodes != ['introducer', 'storageserver']:
            print >>stderr, "Error: Host %s expected nodes are not running. Actual nodes are %r." % (public_host, nodes)
            return False

        return True
    except Exception:
        print >>stderr, "Exception while checking host %s:" % (public_host,)
        traceback.print_exc()
        return False


def check_servers(host_list, monitor_privkey_path, stdout, stderr):
    success = True
    for public_host in host_list:
        print >>stdout, "Checking %r..." % (public_host,)
        success = check_server(public_host, monitor_privkey_path, stdout, stderr) and success

    return success


def compare_servers_to_local(remotepropstuplelist, localstate, stdout, stderr):
    host_list = []
    for rpt in remotepropstuplelist:
        public_host = pubIPextractor(rpt[2])
        host_list.append(public_host)
        if not localstate.has_key(public_host):
            print >>stderr, "Warning: Host %s is not in the list of known servers." % (public_host,)
        else:
            if localstate[public_host][0] != rpt[0]:
                print >>stderr, ("Warning: Host %s launch time changed from %s to %s (probably rebooted)."
                                 % (public_host, localstate[public_host][0], rpt[0]))
            if localstate[public_host][1] != rpt[1]:
                print >>stderr, ("Warning: Host %s changed instance ID from %s to %s."
                                 % (public_host, localstate[public_host][1], rpt[1]))
            del localstate[public_host]

    if localstate:
        print >>stderr, "The following known servers were not found by the AWS query:"
        for key in localstate:
            s = localstate[key]
            print >>stderr, "Host %s Launch time: %s Instance ID: %s" % (key, s[0], s[1])

    return host_list


# The format of each line is RECORD_ADDED_TIME,LAUNCH_TIME,INSTANCE_ID,PUBLIC_HOST

def read_serverinfo(pathtoserverinfo):
    listofinfostrings = FilePath(pathtoserverinfo).getContent().split('\n')
    listofinfotuples = [infostring.split(',')[1:] for infostring in listofinfostrings if infostring]
    return listofinfotuples


def write_serverinfo(pathtoserverinfo, remotepropstuplelist):
    for rpt in remotepropstuplelist:
        append_record(pathtoserverinfo, rpt[0], rpt[1], pubIPextractor(rpt[2]))


MONITORING_EMAIL_SUBJECT = "Monitoring report"

MONITORING_EMAIL_BODY = """Hello, monitoring script here.

The following problems may need investigation:

%s

--\x20
multiservercheck.py
"""

SENDER_DOMAIN = "leastauthority.com"
FROM_EMAIL = "info@leastauthority.com"
FROM_ADDRESS = "Monitoring <%s>" % (FROM_EMAIL,)
TO_EMAIL = FROM_EMAIL
USER_AGENT = "Least Authority Enterprises e-mail sender"

SMTP_HOST = "smtp.googlemail.com"
SMTP_PORT = 25
SMTP_USERNAME = FROM_EMAIL


def send_monitoring_report(errors, password_path='../smtppassword'):
    password = FilePath(password_path).getContent().strip()

    content = MONITORING_EMAIL_BODY % (errors,)
    headers = {
               "From": FROM_ADDRESS,
               "Subject": MONITORING_EMAIL_SUBJECT,
               "User-Agent": USER_AGENT,
               "Content-Type": 'text/plain; charset="utf-8"',
              }

    return send_plain_email(SMTP_HOST, SMTP_USERNAME, password, FROM_EMAIL, TO_EMAIL,
                            content, headers, SENDER_DOMAIN, SMTP_PORT)
