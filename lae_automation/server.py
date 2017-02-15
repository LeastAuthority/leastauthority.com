
"""
-- The EC2Admin public keys, corresponding to the EC2Admin private keys they specify will live on the web server.
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import os, subprocess
from datetime import datetime
utcnow = datetime.utcnow

from pipes import quote as shell_quote
from cStringIO import StringIO

from OpenSSL.crypto import FILETYPE_PEM

from fabric import api
from fabric.context_managers import cd

from twisted.internet.ssl import KeyPair
from twisted.python.filepath import FilePath

from foolscap.api import Tub

from allmydata.util import keyutil

class PathFormatError(Exception):
    pass

STORAGE_ROOT = "/home/customer/storageserver"
INTRODUCER_ROOT = "/home/customer/introducer"

CONFIGURE_TAHOE_PATH = FilePath(__file__).sibling(b"configure-tahoe")

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


def make_external_furl(internal_furl, publichost):
    (prefix, atsign, suffix) = internal_furl.partition('@')
    assert atsign, internal_furl
    (location, slash, swissnum) = suffix.partition('/')
    assert slash, internal_furl
    external_furl = "%s@%s:%s/%s" % (prefix, publichost, INTRODUCER_PORT, swissnum)
    return external_furl


def secrets_to_legacy_format(secrets):
    def nodeid(pem):
        # XXX < warner> we're moving to non-foolscap ed25519 pubkey
        return Tub(certData=pem).tubID.lower()

    return dict(
        user_token=None,
        product_token=None,

        introducer_nodeid=nodeid(secrets["introducer"]["node_pem"]),
        introducer_node_pem=secrets["introducer"]["node_pem"],

        publichost=secrets["storage"]["publichost"],
        privatehost=secrets["storage"]["privatehost"],

        external_introducer_furl=make_external_furl(
            secrets["storage"]["introducer_furl"],
            secrets["storage"]["publichost"],
        ),
        internal_introducer_furl=secrets["storage"]["introducer_furl"],

        bucket_name=secrets["storage"]["bucket_name"],
        server_node_privkey=secrets["storage"]["node_privkey"],
        server_nodeid=nodeid(secrets["storage"]["node_pem"]),
        server_node_pem=secrets["storage"]["node_pem"],

        access_key_id=secrets["storage"]["s3_access_key_id"],
        secret_key=secrets["storage"]["s3_secret_key"],
    )

def marshal_tahoe_configuration(
        introducer_pem,
        storage_pem, storage_privkey,
        bucket_name, publichost, privatehost, introducer_furl,
        s3_access_key_id, s3_secret_key,
        log_gatherer_furl=None,
        stats_gatherer_furl=None,
):
    if log_gatherer_furl is None:
        log_gatherer_furl = ""
    if stats_gatherer_furl is None:
        stats_gatherer_furl = ""
    return dict(
        introducer=dict(
            root=INTRODUCER_ROOT,
            port=INTRODUCER_PORT,
            node_pem=introducer_pem,
            introducer_furl=introducer_furl,
            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        ),
        storage=dict(
            root=STORAGE_ROOT,
            port=SERVER_PORT,
            node_pem=storage_pem,
            node_privkey=storage_privkey,
            bucket_name=bucket_name,
            publichost=publichost,
            privatehost=privatehost,
            introducer_furl=introducer_furl,
            s3_access_key_id=s3_access_key_id,
            s3_secret_key=s3_secret_key,
            log_gatherer_furl=log_gatherer_furl,
            stats_gatherer_furl=stats_gatherer_furl,
        ),
    )


def new_tahoe_configuration(deploy_config, bucketname, publichost, privatehost, introducer_port):
    """
    Create brand new secrets and configuration for use by an
    introducer/storage pair.
    """
    base_name = dict(
        organizationName=b"Least Authority Enterprises",
        organizationalUnitName=b"S4",
        emailAddress=bucketname,
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

    introducer_tub = Tub(certData=pem(keypair, introducer_certificate))
    introducer_tub.setLocation("{}:{}".format(publichost, introducer_port))
    storage_tub = Tub(certData=pem(keypair, storage_certificate))

    return marshal_tahoe_configuration(
        introducer_pem=introducer_tub.getCertData().strip(),

        storage_pem=storage_tub.getCertData().strip(),
        storage_privkey=keyutil.make_keypair()[0] + b"\n",

        bucket_name=bucketname,
        publichost=publichost,
        privatehost=privatehost,
        # The object of the reference is irrelevant.  The furl will
        # get hooked up to something else when Tahoe really runs.
        # Just need to pass something _weak referenceable_!  Which
        # rules out a lot of things...
        introducer_furl=introducer_tub.registerReference(introducer_tub),

        s3_access_key_id=deploy_config.s3_access_key_id,
        s3_secret_key=deploy_config.s3_secret_key,

        log_gatherer_furl=deploy_config.log_gatherer_furl,
        stats_gatherer_furl=deploy_config.stats_gatherer_furl,
    )
