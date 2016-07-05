
"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import os, sys, base64, simplejson, subprocess
from datetime import datetime
utcnow = datetime.utcnow

from pipes import quote as shell_quote
from cStringIO import StringIO
from ConfigParser import SafeConfigParser

from fabric import api
from fabric.context_managers import cd
from fabric.contrib import files

from lae_util.streams import LoggingStream

# The incident gatherer and stats gatherer furls are secret, so they are obtained from
# lae_automation_config.
from lae_automation.config import Config

class PathFormatError(Exception):
    pass

UNATTENDED_UPGRADE_REBOOT_SECONDS = 300
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
    ssl_certificate /home/website/secret_config/rapidssl/server.crt;
    ssl_certificate_key /home/website/secret_config/rapidssl/server.key;

    ssl_session_cache   shared:SSL:10m;
    ssl_session_timeout 10000m;

    ssl_protocols SSLv3 TLSv1;
    ssl_ciphers RC4-SHA:AES128-SHA:DHE-DSS-AES128-SHA:DHE-RSA-AES128-SHA:AES256-SHA:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA;
    ssl_prefer_server_ciphers on;

    add_header Strict-Transport-Security max-age=100000;

    add_header Cache-Control public;
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

INFRASTRUCTURE_CRONTAB = """\
@reboot /home/website/leastauthority.com/start.sh
@reboot /home/website/cronbackupjob.sh
*/10 * * * * /home/website/leastauthority.com/multiservercheck.sh &> /home/website/msccronrunlog
*/10 * * * * /home/website/leastauthority.com/analytics_check.sh &> /home/website/accronrunlog
26 22 * * * /home/website/cronbackupjob.sh
"""

# This isn't used yet because we don't have automated set-up of analytics servers.
ANALYTICS_CRONTAB = """\
@reboot /home/analytics/start.sh
*/10 * * * * /home/analytics/leastauthority.com/website_check.sh &> /home/analytics/wccronrunlog
"""

SSEC2_CRONTAB = """\
@reboot /home/customer/restart.sh
"""



class NotListeningError(Exception):
    pass

TAHOE_LAFS_GIT_REPO_URL = "https://github.com/tahoe-lafs/tahoe-lafs.git"
TAHOE_LAFS_GIT_BRANCH = "2237-cloud-backend-s4"

TAHOE_LAFS_PACKAGE_DEPENDENCIES = [
    'python-dev',
    'python-pip',
    'git-core',
    'libffi6',
    'openssl',
    'libssl1.0.0',
    'python-nevow',
    'python-crypto',
    'python-dateutil',
    'python-foolscap',
    'python-six',
    'python-pycparser',
    'python-unidecode',
    'python-zfec',
    'python-simplejson',
]

EXTRA_INFRASTRUCTURE_PACKAGE_DEPENDENCIES = [
    'python-jinja2',
    'fabric',
    'python-twisted-mail',
    'python-unidecode',
    'python-tz',
    'python-docutils',
    'python-markdown',
]

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
    sudo('chmod -f u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (account_name,) )
    if account_pubkey is None:
        sudo('cp /home/ubuntu/.ssh/authorized_keys /home/%s/.ssh/authorized_keys' % (account_name,))
    else:
        write(account_pubkey, '/home/%s/.ssh/authorized_keys' % (account_name,), use_sudo=True)
    sudo('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(account_name,)))
    sudo('chmod -f 400 /home/%s/.ssh/authorized_keys' % (account_name,))
    sudo('chmod -f 700 /home/%s/.ssh/' % (account_name,))

def apt_install_dependencies(stdout, package_list):
    print >>stdout, "Installing dependencies..."
    for package in package_list:
        print >>stdout, "Installing package: %s" % (package,)
        sudo_apt_get('-y install %s' % (package,))
        print >>stdout, "package: %s installed" % (package,)
    print >>stdout, "Finished installing dependencies..."

def create_and_check_accounts(stdout, stderr, monitor_pubkey, monitor_privkey_path,
                              admin_privkey_path, publichost):
    create_account('customer', None, stdout, stderr)
    create_account('monitor', monitor_pubkey, stdout, stderr)

    # verify that the account exists and can be logged into
    set_host_and_key(publichost, monitor_privkey_path, username="monitor")

    # do the rest of the installation as 'customer', customer doesn't actually have its own ssh keys
    # I don't know if creating one would be useful.XXX
    set_host_and_key(publichost, admin_privkey_path, username="customer")

def get_and_install_tahoe(stdout):
    print >>stdout, "Getting Tahoe-LAFS..."
    run('rm -rf /home/customer/LAFS_source')
    run('git clone -b %s %s LAFS_source' % (TAHOE_LAFS_GIT_BRANCH, TAHOE_LAFS_GIT_REPO_URL))

    with cd('/home/customer/LAFS_source'):
        print >>stdout, "Creating virtualenv..."
        run('virtualenv venv')
        print >>stdout, "Building Tahoe-LAFS..."
        run('venv/bin/pip install -e .[test]')

def create_intro_and_storage_nodes(stdout):
    print >>stdout, "Creating introducer and storage server..."
    run('mkdir -p introducer storageserver')
    run('venv/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.')
    run('venv/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.')

def install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path, stdout,
                   stderr):
    set_host_and_key(publichost, admin_privkey_path)
    update_packages(publichost, admin_privkey_path, stdout, stderr)
    apt_install_dependencies(stdout, TAHOE_LAFS_PACKAGE_DEPENDENCIES)

    sudo_apt_get('-y remove --purge whoopsie')
    get_txaws()
    create_and_check_accounts(stdout, stderr, monitor_pubkey, monitor_privkey_path,
                              admin_privkey_path, publichost)

    get_and_install_tahoe(stdout)

    create_intro_and_storage_nodes(stdout)

    print >>stdout, "Finished server installation."

GIT_DEPLOY_POST_UPDATE_HOOK_TEMPLATE = """#!/bin/bash
cd %s || exit
unset GIT_DIR
exec git update-server-info
"""

def run_git(command):
    return run('/usr/bin/git %s' % (command,))

def make_unique_tag_name(host_IP_address, src_ref_SHA1):
    ''' (str, str) --> str

    Return the unique string id of a tag, to be added to repo.
    '''
    hash_frag = src_ref_SHA1[:8]
    time_tag_name = utcnow().isoformat().split('.')[0].replace(':', '-') + 'Z'
    name = time_tag_name+'_'+host_IP_address+'_'+hash_frag
    return name

def tag_local_repo(host_IP_address, local_repo_gitdir, src_ref_SHA1):
    unique_tag_name = make_unique_tag_name(host_IP_address, src_ref_SHA1)
    command_string = ('/usr/bin/git --git-dir=%s tag %s %s'
                      % (shell_quote(local_repo_gitdir), shell_quote(unique_tag_name), shell_quote(src_ref_SHA1)))
    subprocess.check_call(command_string.split())
    return unique_tag_name


def tag_push_checkout(local_repo_gitdir, src_ref_SHA1, host_IP_address, live_path, git_ssh_path,
                             admin_privkey_path):
    unique_tag = tag_local_repo(host_IP_address, local_repo_gitdir, src_ref_SHA1)
    local_git_push = ['/usr/bin/git',
                      '--git-dir=%s' % (local_repo_gitdir,),
                      'push',
                      'website@%s:%s' % (host_IP_address, live_path),
                      '%s:%s' % (unique_tag, unique_tag)]

    env = {}
    env.update(os.environ)
    env['GIT_SSH'] = git_ssh_path
    env['PRIVATE_KEY'] = admin_privkey_path
    subprocess.check_call(local_git_push, env=env)

    q_unique_tag = shell_quote(unique_tag)
    with cd(live_path):
        run_git('checkout %s' % (q_unique_tag,))
        run_git('checkout -b %s' % (q_unique_tag,))

def setup_git_deploy(host_IP_address, admin_privkey_path, git_ssh_path, live_path, local_repo_gitdir,
                     src_ref_SHA1):
    if live_path.endswith('/') or not os.path.isabs(live_path):
        bad_path = u"%s" % (live_path,)
        error_message = u"live_path is: '%s': but the path must be absolute and not end with /" % (bad_path)
        raise PathFormatError(error_message)

    q_live_path = shell_quote(live_path)
    q_update_hook_path = shell_quote('%s/.git/hooks/post-update' % (live_path,))

    run('rm -rf %s' % (q_live_path,))
    run_git('init %s' % (q_live_path,))
    write(GIT_DEPLOY_POST_UPDATE_HOOK_TEMPLATE % (live_path,), q_update_hook_path)
    run('chmod -f +x %s' % (q_update_hook_path,))

    tag_push_checkout(local_repo_gitdir, src_ref_SHA1, host_IP_address, live_path, git_ssh_path,
                      admin_privkey_path)

def run_unattended_upgrade(api, seconds_for_reboot_pause):
    sudo_apt_get('update')
    sudo('unattended-upgrade --minimal_upgrade_steps')
    api.reboot(seconds_for_reboot_pause)

def run_flapp_web_servers():
    with cd('/home/website/leastauthority.com'):
        # FIXME: make idempotent
        if not files.exists('/home/website/leastauthority.com/flapp'):
            run('flappserver create /home/website/leastauthority.com/flapp')
            run('flappserver add /home/website/leastauthority.com/flapp run-command --accept-stdin /home/website/leastauthority.com /home/website/leastauthority.com/full_signup.sh | tail -1 | cut -d " " -f3 > /home/website/secret_config/signup.furl')
        run('./runsite.sh')

def install_infrastructure_server(publichost, admin_privkey_path, website_pubkey,
                                  leastauth_repo_gitdir, leastauth_commit_hash,
                                  secret_config_repo_gitdir, secret_config_commit_hash,
                                  stdout, stderr):
    """
    This is the code that sets up the infrastructure server.
    This is intended to be idempotent.

    Known sources of non-idempotence:
        - setup_git_deploy
    """
    set_host_and_key(publichost, admin_privkey_path)
    print >>stdout, "Updating server..."
    run_unattended_upgrade(api, UNATTENDED_UPGRADE_REBOOT_SECONDS)
    postfixdebconfstring="""# General type of mail configuration:
# Choices: No configuration, Internet Site, Internet with smarthost, Satellite system, Local only
postfix	postfix/main_mailer_type select	No configuration"""

    print >>stdout, "Installing dependencies..."
    package_list = TAHOE_LAFS_PACKAGE_DEPENDENCIES + EXTRA_INFRASTRUCTURE_PACKAGE_DEPENDENCIES
    apt_install_dependencies(stdout, package_list)
    # From:  https://stripe.com/docs/libraries
    sudo('pip install --index-url https://code.stripe.com --upgrade stripe')
    write(postfixdebconfstring, '/home/ubuntu/postfixdebconfs.txt')
    sudo('debconf-set-selections /home/ubuntu/postfixdebconfs.txt')
    sudo_apt_get('install -y postfix')
    sudo_apt_get('install -y darcs')

    run('wget https://pypi.python.org/packages/source/p/pelican/pelican-3.2.2.tar.gz')
    run('tar zxf pelican-3.2.2.tar.gz')
    with cd('pelican-3.2.2'):
        sudo('python setup.py install')

    create_account('website', website_pubkey, stdout, stderr)

    sudo_apt_get('install -y authbind')
    sudo('touch /etc/authbind/byport/{443,80}')
    sudo('chown website:root /etc/authbind/byport/{443,80}')
    sudo('chmod -f 744 /etc/authbind/byport/{443,80}')

    run('wget -O txAWS-%s.tar.gz %s' % (INSTALL_TXAWS_VERSION, INSTALL_TXAWS_URL))
    run('tar -xzvf txAWS-%s.tar.gz' % (INSTALL_TXAWS_VERSION,))
    with cd('/home/ubuntu/txAWS-%s' % (INSTALL_TXAWS_VERSION,)):
        sudo('python ./setup.py install')

    # patch twisted to send intermediate certs, cf. https://github.com/LeastAuthority/leastauthority.com/issues/6
    sudo("sed --in-place=bak 's/[.]use_certificate_file[(]/.use_certificate_chain_file(/g' $(python -c 'import twisted, os; print os.path.dirname(twisted.__file__)')/internet/ssl.py")

    set_host_and_key(publichost, admin_privkey_path, 'website')
    git_ssh_path = os.path.join(os.path.dirname(leastauth_repo_gitdir), 'git_ssh.sh')
    setup_git_deploy(publichost, admin_privkey_path, git_ssh_path, '/home/website/leastauthority.com',
                     leastauth_repo_gitdir, leastauth_commit_hash)
    setup_git_deploy(publichost, admin_privkey_path, git_ssh_path, '/home/website/secret_config',
                     secret_config_repo_gitdir, secret_config_commit_hash)

    with cd('/home/website/'):
        if not files.exists('signup_logs'):
            run('mkdir signup_logs')
        if not files.exists('secrets'):
            run('mkdir secrets')

    with cd('/home/website/secret_config'):
        run('chmod -f 400 *pem')

    run_flapp_web_servers()
    set_up_crontab(INFRASTRUCTURE_CRONTAB, '/home/website/ctab')

def update_leastauthority_repo(publichost, leastauth_repo_workdir, leastauth_commit_hash, admin_privkey_path):
    set_host_and_key(publichost, admin_privkey_path, 'website')

    leastauth_repo_gitdir = os.path.join(leastauth_repo_workdir, '.git')
    live_path = '/home/website/leastauthority.com'
    git_ssh_path = os.path.join(leastauth_repo_workdir, 'git_ssh.sh')

    tag_push_checkout(leastauth_repo_gitdir, leastauth_commit_hash, publichost, live_path, git_ssh_path,
                      admin_privkey_path)
    run_flapp_web_servers()

def update_blog(publichost, blog_repo_gitdir, blog_commit_hash, git_ssh_path, admin_privkey_path):
    set_host_and_key(publichost, admin_privkey_path, 'website')

    live_path = '/home/website/blog_source'

    tag_push_checkout(blog_repo_gitdir, blog_commit_hash, publichost, live_path, git_ssh_path,
                      admin_privkey_path)
    with cd(live_path):
        run('python /home/website/blog_source/render_blog.py')

def check_branch_and_update_blog(branch, host, blog_repo_workdir, secret_config_repo_workdir, stdout):
    blog_repo_gitdir = os.path.join(blog_repo_workdir, '.git')
    secret_config_repo_gitdir = os.path.join(secret_config_repo_workdir, '.git')
    git_ssh_path = os.path.join(blog_repo_workdir, 'git_ssh.sh')
    admin_privkey_path = os.path.join(secret_config_repo_workdir, 'ec2sshadmin.pem')

    branch_check_command = ['/usr/bin/git', '--git-dir', secret_config_repo_gitdir, 'branch', '--list', branch]

    current_branch = subprocess.check_output(branch_check_command).strip()
    if current_branch != "* %s" % (branch,):
        raise Exception("The %r branch of the secret_config repo must be checked out to run this script." % (branch,))

    blog_repo_HEAD_command = ['/usr/bin/git', '--git-dir', blog_repo_gitdir, 'rev-parse', 'HEAD']
    blog_commit_hash = subprocess.check_output(blog_repo_HEAD_command).strip()
    print >>stdout, blog_commit_hash

    update_blog(host, blog_repo_gitdir, blog_commit_hash, git_ssh_path, admin_privkey_path)


INTRODUCER_PORT = '12345'
SERVER_PORT = '12346'

RESTART_SCRIPT = """#!/bin/sh
cd /home/customer
venv/bin/tahoe restart introducer
venv/bin/tahoe restart storageserver
"""


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
    sudo_apt_get('update')

def set_up_reboot(stdout, stderr):
    print >>stdout, "Setting up introducer and storage server to run on reboot..."
    write(RESTART_SCRIPT, '/home/customer/restart.sh', mode=0750)
    set_up_crontab(SSEC2_CRONTAB, '/home/customer/ctab')

def set_up_crontab(crontab, tmp_path):
    write(crontab, tmp_path)
    run('crontab %s' % (tmp_path,))

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
    run('rm -f /home/customer/introducer/introducer.furl'
             ' /home/customer/introducer/private/introducer.furl'
             ' /home/customer/introducer/logport.furl')
    write(INTRODUCER_PORT + '\n', '/home/customer/introducer/introducer.port')
    write(SERVER_PORT + '\n', '/home/customer/storageserver/client.port')

    if oldsecrets:
        restore_secrets(oldsecrets, 'introducer', stdout, stderr)

    run('venv/bin/tahoe restart introducer && sleep 5')

    internal_introducer_furl = run('cat /home/customer/introducer/private/introducer.furl').strip()
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

    run('chmod -f u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.')
    write(s3_secret_key, '/home/customer/storageserver/private/s3secret', mode=0640)
    if user_token and product_token:
        write(user_token, '/home/customer/storageserver/private/s3usertoken', mode=0640)
        write(product_token, '/home/customer/storageserver/private/s3producttoken', mode=0640)

    print >>stdout, "Starting storage server..."
    run('venv/bin/tahoe restart storageserver && sleep 5')
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


def restore_secrets(secrets, nodetype, stdout, stderr):
    node_pem = secrets.get(nodetype + '_node_pem', '')
    nodeid   = secrets.get(nodetype + '_nodeid', '')
    dirname  = nodetype

    print >>stdout, "Attempting to restore %s identity..." % (nodetype,)
    run('mkdir -p --mode=700 /home/customer/%s/private' % (dirname,))

    if node_pem and nodeid:
        run('chmod -f u+w /home/customer/%s/private/node.pem || echo No existing node.pem.' % (dirname,))
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
