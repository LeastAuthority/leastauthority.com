"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import os, sys, base64, simplejson, subprocess
from cStringIO import StringIO
from ConfigParser import SafeConfigParser

from twisted.python.filepath import FilePath

from fabric import api
from fabric.context_managers import cd

from lae_util.streams import LoggingTeeStream

#The incident gatherer's furl is a secret, so it is obtained from lae_automation_config.
#The stats gatherer's furl is a secret, so it is obtained from lae_automation_config.
from lae_automation.config import Config

TAHOE_CFG_TEMPLATE = """# -*- mode: conf; coding: utf-8 -*-

# This file controls the configuration of the Tahoe node that
# lives in this directory. It is only read at node startup.
# For details about the keys that can be set here, please
# read the 'docs/configuration.rst' file that came with your
# Tahoe installation.


[node]
nickname = %(nickname)s
web.port =
web.static = public_html
tub.location = %(publichost)s:12346,%(privatehost)s:12346
log_gatherer.furl = %(incident_gatherer_furl)s

[client]
# Which services should this client connect to?
introducer.furl = %(introducer_furl)s
helper.furl =
stats_gatherer.furl = %(stats_gatherer_furl)s

[storage]
# Shall this node provide storage service?
enabled = true
backend = s3
s3.access_key_id = %(access_key_id)s
s3.bucket = %(bucket_name)s

[helper]
# Shall this node run a helper service that clients can use?
enabled = false

[drop_upload]
# Shall this node automatically upload files created or modified in a local directory?
enabled = false
local.directory =
"""

class NotListeningError(Exception):
    pass

class InvalidSecrets(Exception):
    pass


INSTALL_TXAWS_VERSION = "0.2.1.post4"
INSTALL_TXAWS_URL = "https://tahoe-lafs.org/source/tahoe-lafs/deps/tahoe-lafs-dep-sdists/txAWS-%s.tar.gz" % (INSTALL_TXAWS_VERSION,)

INSTALL_STATMOVER_VERSION = "2013-04-20T02_25_53+0000"
INSTALL_STATMOVER_PACKAGE = "statmover-%s.tar.gz" % (INSTALL_STATMOVER_VERSION,)
INSTALL_SMCLIENT_VERSION  = "3.2012.07.03.18.15.19-5e73c911653e"

# The default 'pty=True' behaviour is unsafe because, when we are invoked via flapp,
# we don't want the flapp client to be able to influence the ssh remote command's stdin.
# pty=False will cause fabric to echo stdin, but that's fine.

def run(argstring, **kwargs):
    return api.run(argstring, pty=False, **kwargs)

def sudo(argstring, **kwargs):
    return api.sudo(argstring, pty=False, **kwargs)

def set_host_and_key(publichost, ssh_private_keyfile, username="ubuntu"):
    api.env.host_string = '%s@%s' % (username, publichost)
    api.env.reject_unknown_hosts = True
    api.env.key_filename = ssh_private_keyfile
    api.env.abort_on_prompts = True

    try:
        whoami = run('whoami')
    except SystemExit:
        # fabric stupidly aborts if the host is not listening for ssh connections
        # and this is why SystemExit needs to be catchable, zooko ;-)
        raise NotListeningError()
    assert whoami.strip() == username, (whoami, username)

def sudo_apt_get(argstring):
    sudo('apt-get %s' % argstring)

def sudo_easy_install(argstring):
    sudo('easy_install %s' % argstring)

def write(value, remote_path, use_sudo=False, mode=None):
    # There's an incompletely understood interaction between use_sudo and mode.
    # It can result in cryptic remote file names when use_sudo is True and
    # mode is not None.
    return api.put(StringIO(value), remote_path, use_sudo=use_sudo, mode=mode)


def delete_customer(publichost, admin_privkey_path):
    set_host_and_key(publichost, admin_privkey_path)

    sudo('deluser customer')
    sudo('rm -rf /home/customer*')


def create_account(account_name, account_pubkey, stdout, stderr):
    print >>stdout, "Setting up %s account..." % (account_name,)
    sudo('adduser --disabled-password --gecos "" %s || echo Assuming that %s already exists.' % (2*(account_name,)) )
    sudo('mkdir -p /home/%s/.ssh/' % (account_name,) )
    sudo('chown %s:%s /home/%s/.ssh' % (3*(account_name,)) )
    sudo('chmod u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (account_name,) )
    if account_pubkey is None:
        sudo('cp /home/ubuntu/.ssh/authorized_keys /home/%s/.ssh/authorized_keys' % (account_name,))
    else:
        write(account_pubkey, '/home/%s/.ssh/authorized_keys' % (account_name,), use_sudo=True)
    sudo('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(account_name,)))
    sudo('chmod 400 /home/%s/.ssh/authorized_keys' % (account_name,))
    sudo('chmod 700 /home/%s/.ssh/' % (account_name,))


def install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path, stdout,
                   stderr):
    set_host_and_key(publichost, admin_privkey_path)

    print >>stdout, "Updating server..."
    sudo_apt_get('update')
    sudo_apt_get('dist-upgrade -y')

    print >>stdout, "Rebooting server..."
    api.reboot(60)

    print >>stdout, "Installing dependencies..."
    sudo_apt_get('install -y python-dev')
    sudo_apt_get('install -y python-setuptools')
    sudo_apt_get('install -y exim4-base')
    sudo_apt_get('install -y darcs')
    sudo_apt_get('install -y python-foolscap')
    run('wget %s' % (INSTALL_TXAWS_URL,))
    run('tar -xzvf txAWS-%s.tar.gz' % (INSTALL_TXAWS_VERSION,))
    with cd('/home/ubuntu/txAWS-%s' % (INSTALL_TXAWS_VERSION,)):
        sudo('python ./setup.py install')
    create_account('customer', None, stdout, stderr)
    create_account('monitor', monitor_pubkey, stdout, stderr)

    # verify that the account exists and can be logged into
    set_host_and_key(publichost, monitor_privkey_path, username="monitor")

    # do the rest of the installation as 'customer', customer doesn't actually have its own ssh keys
    # I don't know if creating one would be useful.XXX
    set_host_and_key(publichost, admin_privkey_path, username="customer")

    print >>stdout, "Getting Tahoe-LAFS..."
    run('rm -rf /home/customer/LAFS_source')
    run('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source')

    print >>stdout, "Building Tahoe-LAFS..."
    with cd('/home/customer/LAFS_source'):
        run('python ./setup.py build')

    print >>stdout, "Creating introducer and storage server..."
    run('mkdir -p introducer storageserver')
    run('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.')
    run('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.')

    print >>stdout, "Finished server installation."


INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

RESTART_SCRIPT = """#!/bin/sh
cd /home/customer
LAFS_source/bin/tahoe restart introducer
LAFS_source/bin/tahoe restart storageserver
"""


def update_txaws(publichost, admin_privkey_path, stdout, stderr):
    set_host_and_key(publichost, admin_privkey_path)
    run('wget %s' % (INSTALL_TXAWS_URL,))
    run('tar -xzvf txAWS-%s.tar.gz' % (INSTALL_TXAWS_VERSION,))
    with cd('/home/ubuntu/txAWS-%s' % (INSTALL_TXAWS_VERSION,)):
        sudo('python ./setup.py install')


def update_tahoe(publichost, admin_privkey_path, stdout, stderr, do_update_txaws=False):
    set_host_and_key(publichost, admin_privkey_path, username="customer")
    print >>stdout, "Updating Tahoe-LAFS..."
    with cd('/home/customer/LAFS_source'):
        run('bin/tahoe stop ../introducer || echo Assuming introducer is stopped.')
        run('bin/tahoe stop ../storageserver || echo Assuming storage server is stopped.')
        run('darcs pull --all')
    if do_update_txaws:
        update_txaws(publichost, admin_privkey_path, stdout, stderr)
        set_host_and_key(publichost, admin_privkey_path, username="customer")
    with cd('/home/customer/LAFS_source'):
        run('python setup.py build')
    print >>stdout, "Restarting..."
    run('/home/customer/restart.sh')

def register_gatherer(publichost, admin_privkey_path, stdout, stderr, gatherer_type, furl):
    set_host_and_key(publichost, admin_privkey_path, username="customer")
    if gatherer_type == 'incident':
        print >>stdout, "Registering storageserver with %s gatherer." % (gatherer_type,)
        setremoteconfigoption('/home/customer/storageserver/tahoe.cfg', 'node', 'log_gatherer.furl', furl)
        print >>stdout, "Registering introducer with %s gatherer." % (gatherer_type,)
        setremoteconfigoption('/home/customer/introducer/tahoe.cfg', 'node', 'log_gatherer.furl', furl)
    elif gatherer_type == 'stats':
        print >>stdout, "Registering storageserver with %s gatherer." % (gatherer_type,)
        setremoteconfigoption('/home/customer/storageserver/tahoe.cfg', 'client', 'stats_gatherer.furl', furl)

    print >>stdout, "Restarting..."
    run('/home/customer/restart.sh')


def update_packages(publichost, admin_privkey_path, stdout, stderr):
    set_host_and_key(publichost, admin_privkey_path)
    pass

def set_up_reboot(stdout, stderr):
    print >>stdout, "Setting up introducer and storage server to run on reboot..."
    write(RESTART_SCRIPT, '/home/customer/restart.sh', mode=0750)
    write('@reboot /home/customer/restart.sh\n', '/home/customer/ctab')
    run('crontab /home/customer/ctab')


def record_secrets(basefp, publichost, timestamp, admin_privkey_path, raw_stdout, raw_stderr):
    seed = base64.b32encode(os.urandom(20)).rstrip('=').lower()
    logfilename = "%s-%s" % (timestamp.replace(':', ''), seed)

    secretsfp = basefp.child('secrets').child(logfilename)
    logfile = basefp.child('signup_logs').child(logfilename).open('a+')

    stdout = LoggingTeeStream(raw_stdout, logfile, '>')
    stderr = LoggingTeeStream(raw_stderr, logfile, '')

    # This is to work around the fact that fabric echoes all commands and output to sys.stdout.
    # It does have a way to disable that, but not (easily) to redirect it.
    sys.stdout = stderr

    try:
        set_host_and_key(publichost, admin_privkey_path, username="customer")

        print >>stdout, "Reading secrets..."
        introducer_node_pem = run('cat /home/customer/introducer/private/node.pem')
        introducer_nodeid   = run('cat /home/customer/introducer/my_nodeid')
        server_node_pem     = run('cat /home/customer/storageserver/private/node.pem')
        server_nodeid       = run('cat /home/customer/storageserver/my_nodeid')

        tahoe_cfg = run('cat /home/customer/storageserver/tahoe.cfg')
        config = SafeConfigParser()
        config.readfp(StringIO(tahoe_cfg))
        internal_introducer_furl = config.get('client', 'introducer.furl')
        external_introducer_furl = make_external_furl(internal_introducer_furl, publichost)

        access_key_id = config.get('storage', 's3.access_key_id')
        bucket_name = config.get('storage', 's3.bucket')

        secret_key = run('cat /home/customer/storageserver/private/s3secret')
        user_token = run('cat /home/customer/storageserver/private/s3usertoken')
        product_token = run('cat /home/customer/storageserver/private/s3producttoken')

        tub_location = config.get('node', 'tub.location')
        # %(publichost)s:12346,%(privatehost)s:12346
        privatehost = tub_location.partition(',')[2].partition(':')[0]

        print >>stdout, "Writing secrets file..."
        write_secrets_file(secretsfp, {
            'publichost':               publichost,
            'privatehost':              privatehost,
            'access_key_id':            access_key_id,
            'secret_key':               secret_key,
            'user_token':               user_token,
            'product_token':            product_token,
            'bucket_name':              bucket_name,
            'introducer_node_pem':      introducer_node_pem,
            'introducer_nodeid':        introducer_nodeid,
            'server_node_pem':          server_node_pem,
            'server_nodeid':            server_nodeid,
            'internal_introducer_furl': internal_introducer_furl,
            'external_introducer_furl': external_introducer_furl,
        })
    finally:
        stdout.flush()
        stderr.flush()
        logfile.close()


def write_secrets_file(secretsfp, secrets):
    secrets_json = simplejson.dumps(secrets, sort_keys=True, indent=2) + "\n"
    secretsfp.setContent(secrets_json)


def read_secrets_file(secretsfp):
    try:
        secrets_json = secretsfp.getContent()
    except Exception, e:
        raise InvalidSecrets("Error for %r: could not read secrets file: %s" % (secretsfp.path, e))

    try:
        secrets = simplejson.loads(secrets_json)
    except Exception, e:
        # Don't include the message of e since we can't be sure it doesn't contain secrets.
        # The exception class name should be safe.
        raise InvalidSecrets("Error for %r: could not parse secrets file: %s" % (secretsfp.path, e.__class__.__name__))

    needed_fields = ('bucket_name', 'publichost', 'external_introducer_furl', 'server_nodeid')
    for field in needed_fields:
        if field not in secrets:
            raise InvalidSecrets("Error for %r: secrets file did not contain some needed fields.\n"
                                 "Missing fields were %r" % (secretsfp.path, sorted(set(needed_fields) - set(secrets.keys()))))

    return secrets


def make_external_furl(internal_furl, publichost):
    (prefix, atsign, suffix) = internal_furl.partition('@')
    assert atsign, internal_furl
    (location, slash, swissnum) = suffix.partition('/')
    assert slash, internal_furl
    external_furl = "%s@%s:%s/%s" % (prefix, publichost, INTRODUCER_PORT, swissnum)
    return external_furl


def bounce_server(publichost, admin_privkey_path, privatehost, access_key_id,
                              secret_key, user_token, product_token, bucket_name,
                              oldsecrets, stdout, stderr, secretsfp,
                              configpath='../secret_config/lae_automation_config.json'):
    assert isinstance(secretsfp, FilePath)
    nickname = bucket_name

    set_host_and_key(publichost, admin_privkey_path, username="customer")

    print >>stdout, "Starting introducer..."
    run('rm -f /home/customer/introducer/introducer.furl')
    write(INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port')
    write(SERVER_PORT + '\n', '/home/customer/storageserver/client.port')
    run('LAFS_source/bin/tahoe restart introducer && sleep 5')
    internal_introducer_furl = run('cat /home/customer/introducer/introducer.furl').strip()
    assert '\n' not in internal_introducer_furl, internal_introducer_furl

    config = Config(configpath)
    tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': nickname,
                                      'publichost': publichost,
                                      'privatehost': privatehost,
                                      'introducer_furl': internal_introducer_furl,
                                      'access_key_id': access_key_id,
                                      'bucket_name': bucket_name,
                                      'incident_gatherer_furl': str(config.other['incident_gatherer_furl']),
                                      'stats_gatherer_furl': str(config.other['stats_gatherer_furl'])}
    write(tahoe_cfg, '/home/customer/storageserver/tahoe.cfg')
    run('chmod u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.')
    write(secret_key, '/home/customer/storageserver/private/s3secret', mode=0440)
    write(user_token, '/home/customer/storageserver/private/s3usertoken', mode=0440)
    write(product_token, '/home/customer/storageserver/private/s3producttoken', mode=0440)

    if oldsecrets:
        restore_secrets(oldsecrets, stdout, stderr)

    print >>stdout, "Starting storage server..."
    run('LAFS_source/bin/tahoe restart storageserver && sleep 5')
    run('ps -fC tahoe')
    run('netstat -atW')

    set_up_reboot(stdout, stderr)

    introducer_node_pem = run('cat /home/customer/introducer/private/node.pem')
    introducer_nodeid   = run('cat /home/customer/introducer/my_nodeid')
    server_node_pem     = run('cat /home/customer/storageserver/private/node.pem')
    server_nodeid       = run('cat /home/customer/storageserver/my_nodeid')

    print >>stdout, "The introducer and storage server are running."

    external_introducer_furl = make_external_furl(internal_introducer_furl, publichost)

    # The webserver will HTML-escape the FURL in its output, so no need to escape here.
    print >>stdout, """
The settings for your gateway's tahoe.cfg are:

[client]
introducer.furl = %s

shares.needed = 1
shares.happy = 1
shares.total = 1

""" % (external_introducer_furl,)

    write_secrets_file(secretsfp, {
        'publichost':               publichost,
        'privatehost':              privatehost,
        'access_key_id':            access_key_id,
        'secret_key':               secret_key,
        'user_token':               user_token,
        'product_token':            product_token,
        'bucket_name':              bucket_name,
        'introducer_node_pem':      introducer_node_pem,
        'introducer_nodeid':        introducer_nodeid,
        'server_node_pem':          server_node_pem,
        'server_nodeid':            server_nodeid,
        'internal_introducer_furl': internal_introducer_furl,
        'external_introducer_furl': external_introducer_furl,
    })

    return external_introducer_furl

# The value '60000' is the maximum resolution in the x-dimension that a statmover
# chart can be zoomed to.  It is in units of milliseconds.  Since our emission
# source emits once per minute zooming to a higher resolution has no utility, and
# can be confusing.  Therefore we set this value to match our emission rate.
# i.e. 60000 milliseconds = 1 minute
SETUPMETRIC_TEMPLATE = """setup-metric GAUGE Kilobytes %d %s %s"""
RESOLUTION_MILLISECONDS = 60000

def provision_rss_sink(sink_name_suffix, collection_names):
    """we use this function to provision a new sink at statmover."""
    run(SETUPMETRIC_TEMPLATE % (RESOLUTION_MILLISECONDS, sink_name_suffix, ' '.join(collection_names)))


EMIT_CONFIG_TEMPLATE = """[
    {
      "resolution": %d,
      "type": "metric",
      "name": "%s//%s"
    }
]
"""

CRON_EMISSION_SCRIPT = """#!/bin/sh
PATH=/usr/local/bin:${PATH}
cd /home/monitor/statmover/
/home/monitor/statmover/generatevalues.py
"""

GENERATE_SCRIPT = """#! /usr/bin/env python

import subprocess, time, os
import simplejson as json
from twisted.python.filepath import FilePath

STORAGESERVER_PID_PATH = '/home/customer/storageserver/twistd.pid'
EVENT_EMITS_CONFIG_PATH = './eventemissions_config.json'

# Adapted from lae_automation/config.py
def get_emission_list(jsonconfigfile):
    if type(jsonconfigfile) is str:
        configFile = open(jsonconfigfile, 'r')
    try:
        fpos = configFile.tell()
        emitlist = json.load(configFile)
    except json.decoder.JSONDecodeError, e:
        configFile.seek(fpos)
        data = configFile.read()
        e.args = tuple(e.args + (configFile, repr(data),))
        raise
    finally:
        configFile.close()

    assert isinstance(emitlist, list)
    freshlist = []
    for eventemit in emitlist:
        cleandict = {}
        assert isinstance(eventemit, dict), eventemit
        for field in ("resolution", "type", "name"):
            assert field in eventemit, eventemit
            cleandict[field] = str(eventemit.pop(field))
            assert isinstance(cleandict[field], str), field
        assert len(eventemit) == 0, eventemit #Check to see there aren't unexpected fields.
        freshlist.append(cleandict)
    return freshlist

# adapted from pyutil/memutil.py (zooko)
class NotSupportedException(Exception):
    pass

def get_mem_used(process_id_int):
    '''
    This only works on Linux, and only if the /proc/$PID/statm output is the
    same as that in linux kernel 2.6.
    @return: tuple of (bytes of rss memory, bytes of virtual memory) used by this process
    '''
    try:
        import resource
    except ImportError, e:
        raise NotSupportedException(e)
    # sample output from cat /proc/$PID/statm:
    # 14317 3092 832 279 0 2108 0
    a = os.popen("cat /proc/%s/statm 2>/dev/null" % process_id_int).read().split()
    if not a:
        raise NotSupportedException(a)
    virtual_mem_in_page_units = int(a[0])
    resident_set_size_in_page_units = int(a[1])
    return (resident_set_size_in_page_units * resource.getpagesize(), 
            virtual_mem_in_page_units * resource.getpagesize())

# copied from nejucomo
def make_emit_string(name, time, interval, typename, value):
    return json.dumps(make_emit_structure(name, time, interval, typename, value))

# copied from nejucomo
def make_emit_structure(name, time, interval, typename, value):
    if typename == 'metric':
        assert type(value) is int, 'metric emit values must be integers, not %r' % (type(value),)
        typecode = 1
        valuestructure = {'value': value}
    elif typename == 'annotation':
        assert type(value) is unicode, 'annotation emit values must be unicode, not %r' % (type(value),)
        typecode = 3
        valuestructure = {'text': value}
    else:
        raise NotSupportedException( 'Unknown typename not supported: %r' % (typename,) )

    return [
        'EventEmit',
        {'name': name,
         'type': typecode,
         'time': time,
         'resolution': interval,
         typename: valuestructure}]

def get_pid(stringpath_to_pidfile):
    pidstring = FilePath(stringpath_to_pidfile).getContent()
    assert isinstance(pidstring, str), pidstring
    pidint = int(pidstring)
    return pidint

def main():
    emiteventlist = get_emission_list(EVENT_EMITS_CONFIG_PATH)
    PID = get_pid(STORAGESERVER_PID_PATH)
    for event_to_emit in emiteventlist:
        if 'rss' in event_to_emit['name']:
            kbofrss = get_mem_used(PID)[0]/1000 #Perhaps change to emitting bytes.
            emit_time = int(time.time())
            emitstring = make_emit_string(str( event_to_emit['name'] ),
                                          int( emit_time*1000 ),
                                          int( event_to_emit['resolution'] ),
                                          str( event_to_emit['type'] ),
                                          int( kbofrss )  )
            fname = 'emissionlogs/%s' % emit_time
            fh = open(fname, 'w')
            fh.write(emitstring)
            fh.write('\\n')
            fh.close()
            retcode = subprocess.check_call('emit-client --tee --format json'.split(), stdin=open(fname,'r'))
            assert retcode == 0, retcode

if __name__ == '__main__':
    main()
"""

def initialize_statmover_source(publichost, monitor_privkey_path, admin_privkey_path,
                                sinkname_suffix, collection_names):
    EMIT_CONFIG = EMIT_CONFIG_TEMPLATE % (RESOLUTION_MILLISECONDS, sinkname_suffix, '//'.join(collection_names))
    # Set the initial state (make this function idempotent)
    set_host_and_key(publichost, admin_privkey_path, username="ubuntu")
    with cd('/home/monitor'):
        sudo('rm -rf statmover* .saturnalia* ctab emissionscript.sh')

    # Setup up directory structure and scp statmover tarball into it
    set_host_and_key(publichost, monitor_privkey_path, username="monitor")
    path_to_statmover = '../secret_config/'+INSTALL_STATMOVER_PACKAGE
    scpstring = 'scp -i %s %s monitor@%s:' % (monitor_privkey_path, path_to_statmover, publichost)
    subprocess.check_output(scpstring.split())
    run('tar -xzvf %s' % (INSTALL_STATMOVER_PACKAGE,))
    run('mv /home/monitor/statmover/config /home/monitor/.saturnaliaclient')

    # Install the statmover client
    set_host_and_key(publichost, admin_privkey_path, username="ubuntu")
    with cd('/home/monitor/statmover/saturnaliaclient-%s' % (INSTALL_SMCLIENT_VERSION,)):
        sudo('python ./setup.py install')

    set_host_and_key(publichost, monitor_privkey_path, username="monitor")
    # Use the freshly installed setup-metric (part of the client) to provision a sink for this SSEC2
    provision_rss_sink(sinkname_suffix, collection_names)
    # Set up to supply (statmover type-)"source" data to the emit-client
    run('mkdir -p /home/monitor/statmover/emissionlogs')

    write(GENERATE_SCRIPT, '/home/monitor/statmover/generatevalues.py')
    with cd('/home/monitor/statmover/'):
        run('chmod u+x generatevalues.py')

    write(EMIT_CONFIG, '/home/monitor/statmover/eventemissions_config.json')
    # Setup cron for user "monitor" to run every minute sending data to emit-client.
    write(CRON_EMISSION_SCRIPT, '/home/monitor/emissionscript.sh')
    run('chmod u+x /home/monitor/emissionscript.sh')
    write('* * * * * /home/monitor/emissionscript.sh\n', '/home/monitor/ctab')
    run('crontab /home/monitor/ctab')


def restore_secrets(secrets, stdout, stderr):
    if 'introducer_node_pem' in secrets and 'introducer_nodeid' in secrets:
        print >>stdout, "Restoring introducer identity..."
        write(secrets['introducer_node_pem'], '/home/customer/introducer/private/node.pem')
        write(secrets['introducer_nodeid'],   '/home/customer/introducer/my_nodeid')
    else:
        print >>stderr, "Warning: missing field for introducer identity."

    if 'server_node_pem' in secrets and 'server_nodeid' in secrets:
        print >>stdout, "Restoring storage server identity..."
        write(secrets['server_node_pem'], '/home/customer/storageserver/private/node.pem')
        write(secrets['server_nodeid'],   '/home/customer/storageserver/my_nodeid')
    else:
        print >>stderr, "Warning: missing field for storage server identity."


def setremoteconfigoption(pathtoremote, section, option, value):
    """This function expects set_host_and_key have already been run!"""
    #api.get does not appear to actually use StringIO's yet!
    #So I had to use a temp file.
    tempfhandle = open('tempconfigfile','w')
    api.get(pathtoremote, tempfhandle)
    configparser = SafeConfigParser()
    tempfhandle.close()
    tempfhandle = open('tempconfigfile','r')
    configparser.readfp(tempfhandle)
    configparser.set(section, option, value)
    outgoingconfig = StringIO()
    configparser.write(outgoingconfig)
    temppath = pathtoremote+'temp'
    outgoingconfig.seek(0)
    write(outgoingconfig.read(), temppath)
    run('mv '+temppath+' '+pathtoremote)
    os.remove('tempconfigfile')
