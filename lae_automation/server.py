
"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import os, sys, base64, simplejson, subprocess
from cStringIO import StringIO
from ConfigParser import SafeConfigParser

from fabric import api
from fabric.context_managers import cd
from fabric.contrib import files

from lae_util.streams import LoggingStream

# The incident gatherer and stats gatherer furls are secret, so they are obtained from
# lae_automation_config.
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
s3.access_key_id = %(s3_access_key_id)s
s3.bucket = %(bucket_name)s

[helper]
# Shall this node run a helper service that clients can use?
enabled = false

[drop_upload]
# Shall this node automatically upload files created or modified in a local directory?
enabled = false
local.directory =
"""

NGINX_CONFIG = """# You may add here your                                                                                             
# server {                                                                                                          
#       ...                                                                                                         
# }                                                                                                                 
# statements for each of your virtual hosts                                                                         
                                                                                                                    
server {                                                                                                            
    listen 8443 default ssl;                                                                                        
    server_name leastauthority.com;                                                                                 
    ssl on;                                                                                                         
    ssl_certificate /home/website/keys/server.crt;                                                                  
    ssl_certificate_key /home/website/keys/server.key;                                                              
                                                                                                                    
    ssl_session_cache    shared:SSL:10m;                                                                            
    ssl_session_timeout 10000m;                                                                                     
                                                                                                                    
    ssl_protocols SSLv3 TLSv1;                                                                                      
    ssl_ciphers RC4-SHA:AES128-SHA:DHE-DSS-AES128-SHA:DHE-RSA-AES128-SHA:AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-AES25
6-SHA;                                                                                                              
    ssl_prefer_server_ciphers on;                                                                                   
                                                                                                                    
    add_header Strict-Transport-Security max-age=100000;                                                            
                                                                                                                    
    add_header  Cache-Control public;                                                                               
    expires 30d;                                                                                                    
                                                                                                                    
    gzip on;                                                                                                        
    gzip_static on;                                                                                                 
    gzip_comp_level 9;                                                                                              
    gzip_types *;                                                                                                   
                                                                                                                    
    access_log /home/website/mailman/server.log;                                                                    
                                                                                                                    
    # match https://leastauthority.com:8443/cgi-bin/mailman/listinfo/*                                              
    # and ask thttpd to run the CGI for us                                                                          
    location /cgi-bin {                                                                                             
        expires off;                                                                                                
        include proxy_params;                                                                                       
        proxy_pass http://127.0.0.1:8581;                                                                           
    }                                                                                                               
    location /images/mailman {                                                                                      
        alias /var/lib/mailman/icons;                                                                               
    }                                                                                                               
    location /pipermail {                                                                                           
        alias /var/lib/mailman/archives/public;                                                                     
        autoindex on;                                                                                               
    }                                                                                                               
}"""

class NotListeningError(Exception):
    pass

INSTALL_TXAWS_VERSION = "0.2.1.post5"
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
    api.reboot(100)

    print >>stdout, "Installing dependencies..."
    sudo_apt_get('install -y python-dev')
    sudo_apt_get('install -y python-setuptools')
    sudo_apt_get('install -y exim4-base')
    sudo_apt_get('install -y darcs')
    sudo_apt_get('install -y python-foolscap')
    sudo_apt_get('remove -y --purge whoopsie')
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

def run_git(command):
    run('/usr/bin/git %s' % (command,))

GIT_DEPLOY_POST_UPDATE_HOOK_TEMPLATE = """#!/bin/bash
cd %s || exit
unset GIT_DIR
git pull hub master
exec git update-server-info
"""

GIT_DEPLOY_LIVE_POST_COMMIT_HOOK_TEMPLATE = """#!/bin/bash
git push hub
"""

def setup_git_deploy(hostname, live_path, local_repo_path, src_ref):
    "FIXME: make this idempotent (?)"
    if live_path.endswith('/') or not live_path.startswith('/'):
        raise Exception("live_path must be absolute and not end with /")
    hub_path = "%s.git" % (live_path,)
    run_git('init --bare %s' % (hub_path,))
    run_git('init %s' % (live_path,))
    if files.exists( '%s/.git/refs/remotes/hub' % (live_path,) ):
        run_git('--git-dir %s/.git remote rm hub' % (live_path,))
    run_git('--git-dir %s/.git remote add hub %s' % (live_path, hub_path))
    update_hook_path = '%s/hooks/post-update' % (hub_path,)
    write(GIT_DEPLOY_POST_UPDATE_HOOK_TEMPLATE % (live_path,), update_hook_path)
    run('chmod +x %s' % (update_hook_path,))
    live_commit_hook_path = '%s/.git/hooks/post-commit' % (live_path,)
    write(GIT_DEPLOY_LIVE_POST_COMMIT_HOOK_TEMPLATE, live_commit_hook_path)
    run('chmod +x %s' % (live_commit_hook_path,))

    print "live_path is %s" % (live_path,)
    local_git_push = ['/usr/bin/git', 
                        '--git-dir=%s' % (local_repo_path,), 
                        'push',
                        'website@%s:%s' % (hostname, hub_path),
                        '%s:master' % (src_ref,)]
    subprocess.check_call(local_git_push)    

def install_infrastructure_server(publichost, admin_privkey_path, website_pubkey, leastauth_repo, 
                                  la_commit_hash, secretconf_repo, sc_commit_hash, stdout, stderr):
    """
    This is the code that sets up the infrastructure server.
    This is intended to be idempotent.

    Known sources of non-idempotence:
        - setup_git_deploy
    """
    api.env.host_string = '%s@%s' % ('ubuntu', publichost)
    api.env.reject_unknown_hosts = True
    api.env.key_filename = admin_privkey_path
    api.env.abort_on_prompts = True
    print >>stdout, "Updating server..."
    postfixdebconfstring="""# General type of mail configuration:
# Choices: No configuration, Internet Site, Internet with smarthost, Satellite system, Local only
postfix	postfix/main_mailer_type select	No configuration"""
    sudo_apt_get('update')
    sudo_apt_get('-y dist-upgrade')
    sudo_apt_get('-y autoremove')
    print >>stdout, "Rebooting server..."
    api.reboot(300)
    print >>stdout, "Installing dependencies..."
    sudo_apt_get('install -y python-dev python-setuptools git-core python-jinja2 '
                            'python-nevow python-dateutil fabric')
    sudo_easy_install('foolscap')
    write(postfixdebconfstring, '/home/ubuntu/postfixdebconfs.txt')
    sudo('debconf-set-selections /home/ubuntu/postfixdebconfs.txt')  
    sudo_apt_get('install -y postfix')
    sudo_apt_get('install -y darcs')

#    sudo_apt_get('install -y nginx')
#    write(NGINX_CONFIG, '/etc/nginx/sites-enabled/mailman', True)
#    sudo('rm /etc/nginx/sites-enabled/default')
#    sudo('service nginx restart')
    
    create_account('website', website_pubkey, stdout, stderr)

    sudo_apt_get('install -y authbind')
    sudo('touch /etc/authbind/byport/{443,80}')
    sudo('chown website:root /etc/authbind/byport/{443,80}')
    sudo('chmod 744 /etc/authbind/byport/{443,80}')
    
    run('wget -O txAWS-%s.tar.gz %s' % (INSTALL_TXAWS_VERSION, INSTALL_TXAWS_URL))
    run('tar -xzvf txAWS-%s.tar.gz' % (INSTALL_TXAWS_VERSION,))
    with cd('/home/ubuntu/txAWS-%s' % (INSTALL_TXAWS_VERSION,)):
        sudo('python ./setup.py install')

    set_host_and_key(publichost, admin_privkey_path, 'website')
    setup_git_deploy(publichost, '/home/website/leastauthority.com', leastauth_repo, la_commit_hash)
    setup_git_deploy(publichost, '/home/website/secret_config', secretconf_repo, sc_commit_hash)

    with cd('/home/website/leastauthority.com'):
        #FIXME: make idempotent
        if not files.exists('/home/website/leastauthority.com/flapp'):
            run('flappserver create /home/website/leastauthority.com/flapp')
            run('flappserver add /home/website/leastauthority.com/flapp run-command --accept-stdin --send-stdout /home/website/leastauthority.com /home/website/leastauthority.com/full_signup.py | tail -1 > /home/website/secret_config/signup.furl')
        run('./runsite.sh')

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
    dirfp = basefp.child('secrets').child('active_SSEC2s')
    try:
        dirfp.makedirs()
    except OSError:
        pass
    secretsfile = dirfp.child(logfilename).open('a+')
    logfile = basefp.child('signup_logs').child(logfilename).open('a+')

    stdout = LoggingStream(logfile, '>')
    stderr = LoggingStream(logfile, '')

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
        server_node_privkey = run('if [[ -e /home/customer/storageserver/private/node.privkey ]];'
                                  ' then cat /home/customer/storageserver/private/node.privkey; fi')

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
            'server_node_privkey':      server_node_privkey,
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


def bounce_server(publichost, admin_privkey_path, privatehost, s3_access_key_id, s3_secret_key,
                  user_token, product_token, bucket_name, oldsecrets, stdout, stderr, secretsfile,
                  configpath='../secret_config/lae_automation_config.json'):
    nickname = bucket_name

    set_host_and_key(publichost, admin_privkey_path, username="customer")

    print >>stdout, "Starting introducer..."
    run('rm -f /home/customer/introducer/introducer.furl /home/customer/introducer/logport.furl')
    write(INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port')
    write(SERVER_PORT + '\n', '/home/customer/storageserver/client.port')

    if oldsecrets:
        restore_secrets(oldsecrets, 'introducer', stdout, stderr)

    run('LAFS_source/bin/tahoe restart introducer && sleep 5')

    internal_introducer_furl = run('cat /home/customer/introducer/introducer.furl').strip()
    assert '\n' not in internal_introducer_furl, internal_introducer_furl

    config = Config(configpath)
    tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': nickname,
                                      'publichost': publichost,
                                      'privatehost': privatehost,
                                      'introducer_furl': internal_introducer_furl,
                                      's3_access_key_id': s3_access_key_id,
                                      'bucket_name': bucket_name,
                                      'incident_gatherer_furl': str(config.other['incident_gatherer_furl']),
                                      'stats_gatherer_furl': str(config.other['stats_gatherer_furl'])}
    write(tahoe_cfg, '/home/customer/storageserver/tahoe.cfg')

    if oldsecrets:
        restore_secrets(oldsecrets, 'storageserver', stdout, stderr)

    run('chmod u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.')
    write(s3_secret_key, '/home/customer/storageserver/private/s3secret', mode=0640)
    if user_token and product_token:
        write(user_token, '/home/customer/storageserver/private/s3usertoken', mode=0640)
        write(product_token, '/home/customer/storageserver/private/s3producttoken', mode=0640)

    print >>stdout, "Starting storage server..."
    run('LAFS_source/bin/tahoe restart storageserver && sleep 5')
    run('ps -fC tahoe')
    run('netstat -atW')

    set_up_reboot(stdout, stderr)

    # FIXME: eliminate code duplication with record_secrets.
    introducer_node_pem = run('cat /home/customer/introducer/private/node.pem')
    introducer_nodeid   = run('cat /home/customer/introducer/my_nodeid')
    server_node_pem     = run('cat /home/customer/storageserver/private/node.pem')
    server_nodeid       = run('cat /home/customer/storageserver/my_nodeid')
    server_node_privkey = run('if [[ -e /home/customer/storageserver/private/node.privkey ]];'
                              ' then cat /home/customer/storageserver/private/node.privkey; fi')

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
        'access_key_id':            s3_access_key_id,
        'secret_key':               s3_secret_key,
        'user_token':               user_token,
        'product_token':            product_token,
        'bucket_name':              bucket_name,
        'introducer_node_pem':      introducer_node_pem,
        'introducer_nodeid':        introducer_nodeid,
        'server_node_pem':          server_node_pem,
        'server_nodeid':            server_nodeid,
        'server_node_privkey':      server_node_privkey,
        'internal_introducer_furl': internal_introducer_furl,
        'external_introducer_furl': external_introducer_furl,
    })
    secretsfile.flush()

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

# Adapted from lae_automation/config.py (davidsarah)
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
        assert len(eventemit) == 0, eventemit   # Check to see there aren't unexpected fields.
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
            kbofrss = get_mem_used(PID)[0]/1000   # Perhaps change to emitting bytes.
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
    scp_list = ['scp',
                '-i',
                monitor_privkey_path,
                path_to_statmover,
                'monitor@%s:' % (publichost,)]
    subprocess.check_output(scp_list)
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


def restore_secrets(secrets, nodetype, stdout, stderr):
    node_pem = secrets.get(nodetype + '_node_pem', '')
    nodeid   = secrets.get(nodetype + '_nodeid', '')
    dirname  = nodetype

    print >>stdout, "Attempting to restore %s identity..." % (nodetype,)
    run('mkdir -p --mode=700 /home/customer/%s/private' % (dirname,))

    if node_pem and nodeid:
        run('chmod u+w /home/customer/%s/private/node.pem || echo No existing node.pem.' % (dirname,))
        write(node_pem, '/home/customer/%s/private/node.pem' % (dirname,), mode=0640)
        write(nodeid,   '/home/customer/%s/my_nodeid' % (dirname,))
    else:
        print >>stderr, "Warning: missing field(s) for %s identity." % (nodetype,)

    if nodetype == 'storageserver':
        # This is used by versions of Tahoe-LAFS that support signed introductions.
        server_node_privkey = secrets.get('server_node_privkey', '')
        if server_node_privkey:
            write(server_node_privkey, '/home/customer/%s/private/node.privkey' % (dirname,), mode=0640)
        else:
            print >>stderr, "Warning: not restoring server's node.privkey."


def setremoteconfigoption(pathtoremote, section, option, value):
    """This function expects set_host_and_key to have already been run!"""
    # api.get does not appear to actually use StringIO instances yet, so this uses a temp file.
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
