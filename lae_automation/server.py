"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import os, sys, base64, simplejson
from cStringIO import StringIO
from ConfigParser import SafeConfigParser
from twisted.python.filepath import FilePath

from fabric import api
from fabric.context_managers import cd

from lae_util.streams import LoggingTeeStream


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

[client]
# Which services should this client connect to?
introducer.furl = %(introducer_furl)s
helper.furl =

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


INSTALL_TXAWS_VERSION = "0.2.1.post4"
INSTALL_TXAWS_URL = "https://leastauthority.com/static/patches/txAWS-%s.tar.gz" % (INSTALL_TXAWS_VERSION,)


# The default 'pty=True' behaviour is unsafe because, when we are invoked via flapp,
# we don't want the flapp client to be able to influence the ssh remote command's stdin.
# pty=False will cause fabric to echo stdin, but that's fine.

def run(argstring, **kwargs):
    return api.run(argstring, pty=False, **kwargs)

def sudo(argstring, **kwargs):
    return api.sudo(argstring, pty=False, **kwargs)

def set_host_and_key(publichost, ssh_private_keyfile, username="ubuntu"):
    api.env.host_string = '%s@%s' % (username, publichost)
    api.env.reject_unknown_hosts = False  # FIXME allows MITM attacks
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

def install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path, stdout, stderr):
    set_host_and_key(publichost, admin_privkey_path)

    print >>stdout, "Updating server..."
    sudo_apt_get('update')
    sudo_apt_get('upgrade -y')
    sudo_apt_get('install -y linux-ec2 linux-image-ec2')
    sudo("dpkg -P consolekit")

    print >>stdout, "Rebooting server..."
    api.reboot(60)

    print >>stdout, "Installing dependencies..."
    sudo_apt_get('install -y python-dev')
    sudo_apt_get('install -y python-setuptools')
    sudo_apt_get('install -y exim4-base')
    sudo_apt_get('install -y darcs')
    sudo_easy_install('foolscap')
    run('wget %s' % (INSTALL_TXAWS_URL,))
    run('tar -xzvf txAWS-%s.tar.gz' % (INSTALL_TXAWS_VERSION,))
    with cd('/home/ubuntu/txAWS-%s' % (INSTALL_TXAWS_VERSION,)):
        sudo('python ./setup.py install')
    create_account('customer', None, stdout, stderr)
    create_account('monitor', monitor_pubkey, stdout, stderr)

    # this also checks that creating the monitor account worked
    set_up_monitors(publichost, monitor_privkey_path, stdout, stderr)

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


def set_up_monitors(publichost, monitor_privkey_path, stdout, stderr):
    set_host_and_key(publichost, monitor_privkey_path, username="monitor")
    print >>stdout, "Getting monitoring code..."
    run('rm -rf /home/monitor/monitors')
    run('darcs get --lazy https://leastauthority.com/static/source/monitors')
    run('chmod +x /home/monitor/monitors/*')


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


def update_packages(publichost, admin_privkey_path, stdout, stderr):
    set_host_and_key(publichost, admin_privkey_path)
    print >>stdout, "Removing unneeded packages..."
    sudo("dpkg -P consolekit")


def set_up_reboot(stdout, stderr):
    print >>stdout, "Setting up introducer and storage server to run on reboot..."
    write(RESTART_SCRIPT, '/home/customer/restart.sh', mode=0750)
    write('@reboot /home/customer/restart.sh\n', '/home/customer/ctab')
    run('crontab /home/customer/ctab')


def record_secrets(publichost, timestamp, admin_privkey_path, raw_stdout, raw_stderr):
    seed = base64.b32encode(os.urandom(20)).rstrip('=').lower()
    logfilename = "%s-%s" % (timestamp.replace(':', ''), seed)

    secretsfile = FilePath('secrets').child(logfilename).open('a+')
    logfile = FilePath('signup_logs').child(logfilename).open('a+')

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
        config = SafeConfigParser(tahoe_cfg, StringIO(tahoe_cfg))
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
        print >>secretsfile, simplejson.dumps({
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
        secretsfile.close()
        logfile.close()


def make_external_furl(internal_furl, publichost):
    (prefix, atsign, suffix) = internal_furl.partition('@')
    assert atsign, internal_furl
    (location, slash, swissnum) = suffix.partition('/')
    assert slash, internal_furl
    external_furl = "%s@%s:%s/%s" % (prefix, publichost, INTRODUCER_PORT, swissnum)
    return external_furl


def bounce_server(publichost, admin_privkey_path, privatehost, access_key_id, secret_key, user_token, product_token, bucket_name,
                  stdout, stderr, secretsfile):
    nickname = bucket_name

    set_host_and_key(publichost, admin_privkey_path, username="customer")

    print >>stdout, "Starting introducer..."
    run('rm -f /home/customer/introducer/introducer.furl')
    write(INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port')
    write(SERVER_PORT + '\n', '/home/customer/storageserver/client.port')
    run('LAFS_source/bin/tahoe restart introducer && sleep 5')
    internal_introducer_furl = run('cat /home/customer/introducer/introducer.furl').strip()
    assert '\n' not in internal_introducer_furl, internal_introducer_furl

    tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': nickname,
                                      'publichost': publichost,
                                      'privatehost': privatehost,
                                      'introducer_furl': internal_introducer_furl,
                                      'access_key_id': access_key_id,
                                      'bucket_name': bucket_name}
    write(tahoe_cfg, '/home/customer/storageserver/tahoe.cfg')
    run('chmod u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.')
    write(secret_key, '/home/customer/storageserver/private/s3secret', mode=0440)
    write(user_token, '/home/customer/storageserver/private/s3usertoken', mode=0440)
    write(product_token, '/home/customer/storageserver/private/s3producttoken', mode=0440)

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

    print >>secretsfile, simplejson.dumps({
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
    secretsfile.flush()

    return external_introducer_furl


def notify_zenoss(EC2pubIP, zenoss_IP, zenoss_privkey_path):
    zenbatchloadstring ='/Devices/Server/SSH/Linux\n%s setManageIp="%s"\n' % (EC2pubIP, EC2pubIP)
    loadfiledirname = '/home/zenoss/loadfiles/'
    loadfilebasename = 'zbatch_%s' % (EC2pubIP,)
    remotepath = loadfiledirname + loadfilebasename
    set_host_and_key(zenoss_IP, zenoss_privkey_path, username='zenoss')
    write(zenbatchloadstring, remotepath)
    run('/usr/local/zenoss/zenoss/bin/zenbatchload %s' % (remotepath,))


def set_remote_config_option(pathtoremote, section, option, value):
    """This function expects set_host_and_key have already been run!"""
    incomingconfig = StringIO()
    api.get(pathtoremote, incomingconfig)
    config = SafeConfigParser.readfp(incomingconfig)
    config.set(section, option, value)
    outgoingconfig = StringIO()
    config.write(outgoingconfig)
    write(outgoingconfig.getvalue(), pathtoremote)
    # FIXME: move this to caller
    run('/home/customer/restart.sh')
