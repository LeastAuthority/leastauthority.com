
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

from twisted.python.filepath import FilePath

from lae_util.streams import LoggingStream

class PathFormatError(Exception):
    pass

STORAGE_ROOT = "/home/customer/storageserver"
INTRODUCER_ROOT = "/home/customer/introducer"

CONFIGURE_TAHOE_PATH = FilePath(__file__).sibling(b"configure-tahoe")
RECORD_SECRETS_PATH = FilePath(__file__).sibling(b"record-secrets")

UNATTENDED_UPGRADE_REBOOT_SECONDS = 300

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
    'libffi-dev',
    'openssl',
    'libssl-dev',
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

    with cd('/home/customer'):
        print >>stdout, "Creating virtualenv..."
        run('virtualenv venv')
        print >>stdout, "Building Tahoe-LAFS..."
        run('venv/bin/pip install --find-links=https://tahoe-lafs.org/deps -e LAFS_source[test]')

def create_intro_and_storage_nodes(stdout):
    print >>stdout, "Creating introducer and storage server..."
    run(
        'venv/bin/tahoe create-introducer {} || '
        'echo Assuming that introducer already exists.'.format(
            INTRODUCER_ROOT
        )
    )
    run(
        'venv/bin/tahoe create-node {} || '
        'echo Assuming that storage server already exists.'.format(
            STORAGE_ROOT
        )
    )

def install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path, stdout,
                   stderr):
    set_host_and_key(publichost, admin_privkey_path)
    update_packages(publichost, admin_privkey_path, stdout, stderr)
    apt_install_dependencies(stdout, TAHOE_LAFS_PACKAGE_DEPENDENCIES)

    sudo_apt_get('-y remove --purge whoopsie')
    sudo('pip install virtualenv')
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


def _record_secrets(publichost):
    api.put(
        RECORD_SECRETS_PATH.path,
        remote_path="/tmp/record-secrets",
        mode=0500,
    )
    secrets = simplejson.loads(run(
        "/tmp/record-secrets /home/customer/introducer /home/customer/storageserver"
    ))
    secrets["external_introducer_furl"] = make_external_furl(
        secrets["internal_introducer_furl"], publichost,
    )

    # %(publichost)s:12346,%(privatehost)s:12346
    secrets["privatehost"] = secrets.pop("tub_location").partition(',')[2].partition(':')[0]
    secrets["publichost"] = publichost

    # TLoS3 only.
    secrets["user_token"] = None
    secrets["product_token"] = None

    return secrets


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
        secrets = _record_secrets(publichost)

        print >>stdout, "Writing secrets file..."
        print >>secretsfile, simplejson.dumps(secrets)
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


def marshal_tahoe_configuration(
        introducer_pem, introducer_node_id,
        storage_pem, storage_privkey, storage_node_id,
        bucket_name, publichost, privatehost, introducer_furl,
        s3_access_key_id, s3_secret_key,
):
    return dict(
        introducer=dict(
            root=INTRODUCER_ROOT,
            port=INTRODUCER_PORT,
            node_pem=introducer_pem,
            node_id=introducer_node_id,
        ),
        storage=dict(
            root=STORAGE_ROOT,
            port=SERVER_PORT,
            node_pem=storage_pem,
            node_privkey=storage_privkey,
            node_id=storage_node_id,
            bucket_name=bucket_name,
            publichost=publichost,
            privatehost=privatehost,
            introducer_furl=introducer_furl,
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
        ),
    )

def bounce_server(publichost, admin_privkey_path, privatehost, s3_access_key_id, s3_secret_key,
                  user_token, product_token, bucket_name, oldsecrets, stdout, stderr, secretsfile,
                  configpath=None):
    set_host_and_key(publichost, admin_privkey_path, username="customer")

    if oldsecrets is None:
        configuration = new_tahoe_configuration(
            nickname=bucket_name,
            bucket_name=bucket_name,
            publichost=publichost,
            privatehost=privatehost,
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
        )
    else:
        configuration = marshal_tahoe_configuration(
            introducer_pem=oldsecrets["introducer_node_pem"],
            introducer_node_id=oldsecrets["introducer_my_nodeid"],
            storage_pem=oldsecrets["storageserver_node_pem"],
            storage_privkey=oldsecrets["server_node_privkey"],
            storage_node_id=oldsecrets["storageserver_my_nodeid"],
            bucket_name=bucket_name,
            publichost=publichost,
            privatehost=privatehost,
            introducer_furl=oldsecrets["internal_introducer_furl"],
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
        )

    api.put(
        local_path=CONFIGURE_TAHOE_PATH.path,
        remote_path="/tmp/configure-tahoe",
        mode=0500,
    )
    api.put(
        local_path=StringIO(simplejson.dumps(configuration)),
        remote_path="/tmp/tahoe-config.json",
        mode=0400,
    )
    print >>stdout, "Configuring Tahoe-LAFS..."
    api.run("/tmp/configure-tahoe < /tmp/tahoe-config.json")

    print >>stdout, "Restarting introducer..."
    run('venv/bin/tahoe restart introducer && sleep 5')

    print >>stdout, "Restarting storage server..."
    run('venv/bin/tahoe restart storageserver && sleep 5')

    set_up_reboot(stdout, stderr)

    secrets = _record_secrets(publichost)

    print >>stdout, "The introducer and storage server are running."

    # The webserver will HTML-escape the FURL in its output, so no need to escape here.
    print >>stdout, """
The settings for your gateway's tahoe.cfg are:

[client]
introducer.furl = %s

shares.needed = 1
shares.happy = 1
shares.total = 1

""" % (secrets["external_introducer_furl"],)

    print >>secretsfile, simplejson.dumps(secrets)
    secretsfile.flush()

    return secrets["external_introducer_furl"]


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


def new_tahoe_configuration(nickname, bucket_name, publichost, privatehost, s3_access_key_id, s3_secret_key):
    """
    Create brand new secrets and configuration for use by an
    introducer/storage pair.

    @param nickname: The nickname of the storage node which will be
        created.  Also, the common name put into the certificate for
        the nodes.

    @type nickname: bytes
    """
    from os import urandom
    from OpenSSL.crypto import FILETYPE_PEM
    from twisted.internet.ssl import KeyPair

    base_name = dict(
        organizationName=b"Least Authority Enterprises",
        organizationalUnitName=b"S4",
        emailAddress=nickname,
    )

    keypair = KeyPair.generate(size=2048)
    introducer_certificate = keypair.selfSignedCert(
        serialNumber=1,
        commonName=b"introducer",
        **base_name
    )
    storage_certificate = keypair.selfSignedCert(
        serialNumber=1,
        commonName=b"storage",
        **base_name
    )
    def pem(key, cert):
        return b"\n".join((key.dump(FILETYPE_PEM), cert.dump(FILETYPE_PEM)))

    def new_node_id():
        return base64.b32encode(urandom(12)).lower().strip("=")

    from foolscap.api import Tub
    introducer_tub = Tub(certData=pem(keypair, introducer_certificate))
    introducer_tub.setLocation(publichost)
    storage_tub = Tub(certData=pem(keypair, storage_certificate))

    return marshal_tahoe_configuration(
        introducer_pem=introducer_tub.getCertData(),
        introducer_node_id=new_node_id(),

        storage_pem=storage_tub.getCertData(),
        storage_privkey=keypair.dump(FILETYPE_PEM),
        storage_node_id=new_node_id(),

        bucket_name=bucket_name,
        publichost=publichost,
        privatehost=privatehost,
        # The object of the reference is irrelevant.  The furl will
        # get hooked up to something else when Tahoe really runs.
        # Just need to pass something _weak referenceable_!  Which
        # rules out a lot of things...
        introducer_furl=introducer_tub.registerReference(introducer_tub),

        s3_access_key_id=s3_access_key_id, s3_secret_key=s3_secret_key,
    )
