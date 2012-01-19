"""
"""

from cStringIO import StringIO
import simplejson

from fabric import api
from fabric.context_managers import cd

class NotListeningError(Exception):
    pass


NGINXCONFIGFILESTRING = """server {
    listen 443 default ssl;
    server_name lambda4.leastauthority.com;
    ssl on;
    ssl_certificate /home/zenoss/keys/server.crt;
    ssl_certificate_key /home/zenoss/keys/server.key;
    ssl_session_timeout 20m;
    ssl_protocols SSLv3 TLSv1;
    ssl_ciphers RSA:HIGH;
    ssl_prefer_server_ciphers on;

    access_log /home/zenoss/logs/server.log;

    location / {
        rewrite ^(.*)$ /VirtualHostBase/https/lambda4.leastauthority.com:443$1 break;
        proxy_pass http://127.0.0.1:8080;
    }
}"""

NGINXCONFIGFILENAME = """/etc/nginx/sites-enabled/default"""
# The default 'pty=True' behaviour is unsafe because, when we are invoked via flapp,
# we don't want the flapp client to be able to influence the ssh remote command's stdin.
# pty=False will cause fabric to echo stdin, but that's fine.

def run(argstring, **kwargs):
    return api.run(argstring, pty=False, **kwargs)

def sudo(argstring, **kwargs):
    return api.sudo(argstring, pty=False, **kwargs)

def set_host_and_key(public_host, ssh_private_keyfile, username="ubuntu"):
    api.env.host_string = '%s@%s' % (username, public_host)
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

def create_account(account_name, account_pubkey, stdout, stderr):
    print >>stdout, "Setting up %s account..." % (account_name,)
    sudo('adduser --disabled-password --gecos "" %s || echo Assuming that %s already exists.' % (2*(account_name,)) )
    sudo('mkdir -p /home/%s/.ssh/' % (account_name,) )
    sudo('chown %s:%s /home/%s/.ssh' % (3*(account_name,)) )
    sudo('chmod u+w /home/%s/.ssh/authorized_keys || echo Assuming there is no existing authorized_keys file.' % (account_name,) )
    if account_pubkey is None:
        sudo('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys')
    else:
        write(account_pubkey, '/home/%s/.ssh/authorized_keys' % (account_name,), use_sudo=True)
    sudo('chown %s:%s /home/%s/.ssh/authorized_keys' % (3*(account_name,)))
    sudo('chmod 400 /home/%s/.ssh/authorized_keys' % (account_name,))
    sudo('chmod 700 /home/%s/.ssh/' % (account_name,))

def install_nginxandzenoss():
    set_host_and_key(public_host, admin_privkey_path)

    print >>stdout, "Updating server..."
    sudo_apt_get('update')
    sudo_apt_get('upgrade -y')
    sudo_apt_get('install -y nginx')

    run('mkdir temp_for_zenossdeb')
    with cd('temp_for_zenossdeb'):
        run('wget http://dev.zenoss.org/deb/dists/main/stable/binary-i386/zenoss-stack_3.2.1_i386.deb')
        run('dpkg -i zenoss-stack_3.2.1_i386.deb')

    #XXX Below here are left--overs...
    run('tar -xzvf txAWS-0.2.1.post2.tar.gz')



    create_account('monitor', monitor_pubkey, stdout, stderr)

    # check that creating the monitor account worked
    set_host_and_key(public_host, monitor_privkey_path, username="monitor")

    # do the rest of the installation as 'customer', customer doesn't actually have its own ssh keys
    # I don't know if creating one would be useful.XXX
    set_host_and_key(public_host, admin_privkey_path, username="customer")

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


def set_up_reboot(stdout, stderr):
    print >>stdout, "Setting up introducer and storage server to run on reboot..."
    write(RESTART_SCRIPT, '/home/customer/restart.sh', mode=0750)
    write('@reboot /home/customer/restart.sh\n', '/home/customer/ctab')
    run('crontab /home/customer/ctab')
