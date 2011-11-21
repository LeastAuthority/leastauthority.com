"""
-- The developers public keys, corresponding to which ever private keys they specify will live on the web server.  
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import sys
from cStringIO import StringIO

from fabric.api import sudo, env, reboot, run, put


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
tub.location = %(public_ip)s:12346,%(private_ip)s:12346

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
upload.dircap =
local.directory =
"""

def set_ip_and_key(public_ip, key_filename, username="ubuntu"):
    env.host_string = '%s@%s' % (username, public_ip)
    env.reject_unknown_hosts = False  # FIXME allows MITM attacks
    env.key_filename = key_filename
    assert run('whoami').strip() == username

def sudo_apt_get(argstring):
    sudo('apt-get %s' % argstring)

def sudo_easy_install(argstring):
    sudo('easy_install %s' % argstring)

def write(remote_path, value, mode=None):
    return put(StringIO(value), remote_path, mode=mode)


def delete_customer(public_ip, key_filename):
    set_ip_and_key(public_ip, key_filename)

    sudo('deluser customer')
    sudo('rm -rf /home/customer*')


def install_server(public_ip, key_filename):
    set_ip_and_key(public_ip, key_filename)

    sudo_apt_get('update')
    sudo_apt_get('upgrade -y')
    sudo_apt_get('install -y linux-ec2 linux-image-ec2')
    reboot(60)
    sudo_apt_get('install -y python-dev')
    sudo_apt_get('install -y python-setuptools')
    sudo_apt_get('install -y exim4-base')
    sudo_apt_get('install -y darcs')
    sudo_easy_install('foolscap')
    run('wget https://leastauthority.com/content/static/patches/txAWS-0.2.1.post2.tar.gz')
    run('tar -xzvf txAWS-0.2.1.post2.tar.gz')
    sudo("/bin/sh -c 'cd /home/ubuntu/txAWS-0.2.1.post2 && python ./setup.py install'")
    sudo('adduser --disabled-password --gecos "" customer || echo Assuming that user already exists.')
    sudo('mkdir -p /home/customer/.ssh/')
    sudo('chown customer:customer /home/customer/.ssh')
    sudo('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys')
    sudo('chown customer:customer /home/customer/.ssh/authorized_keys')
    sudo('chmod 400 /home/customer/.ssh/authorized_keys')
    sudo('chmod 700 /home/customer/.ssh/')

    set_ip_and_key(public_ip, key_filename, username="customer")

    run('rm -rf /home/customer/LAFS_source')
    run('darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source')
    run("/bin/sh -c 'cd LAFS_source && python ./setup.py build'")
    run('mkdir -p introducer storageserver')
    run('LAFS_source/bin/tahoe create-introducer introducer || echo Assuming that introducer already exists.')
    run('LAFS_source/bin/tahoe create-node storageserver || echo Assuming that storage server already exists.')


def bounce_server(public_ip, key_filename, nickname, private_ip, access_key_id, secret_key, user_token, product_token, bucket_name):
    set_ip_and_key(public_ip, key_filename, username="customer")

    run('rm -f /home/customer/introducer/introducer.furl')
    write('/home/customer/introducer/introducer.port', '12345\n')
    write('/home/customer/storageserver/client.port', '12346\n')
    run('LAFS_source/bin/tahoe restart introducer && sleep 5')
    introducer_furl = run('cat /home/customer/introducer/introducer.furl').strip()
    assert '\n' not in introducer_furl, introducer_furl

    tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': nickname,
                                      'public_ip': public_ip,
                                      'private_ip': private_ip,
                                      'introducer_furl': introducer_furl,
                                      'access_key_id': access_key_id,
                                      'bucket_name': bucket_name}
    write('/home/customer/storageserver/tahoe.cfg', tahoe_cfg)
    run('chmod u+w /home/customer/storageserver/private/s3* || echo Assuming there are no existing s3 secret files.')
    write('/home/customer/storageserver/private/s3secret', secret_key, mode=0440)
    write('/home/customer/storageserver/private/s3usertoken', user_token, mode=0440)
    write('/home/customer/storageserver/private/s3producttoken', product_token, mode=0440)

    run('LAFS_source/bin/tahoe restart storageserver && sleep 5')
    run('ps -fC tahoe')
    run('netstat -at')


if len(sys.argv) < 10:
    print "Usage: python configure.py PUBLIC_IP KEY_FILE NICKNAME PRIVATE_IP ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME [--no-install]"
    print "Happy configuring!"
    sys.exit(1)

public_ip = sys.argv[1]
key_filename = sys.argv[2]
nickname = sys.argv[3]
private_ip = sys.argv[4]
access_key_id = sys.argv[5]
secret_key = sys.argv[6]
user_token = sys.argv[7]
product_token = sys.argv[8]
bucket_name = sys.argv[9]

if "--no-install" not in sys.argv:
    install_server(public_ip, key_filename)

bounce_server(public_ip, key_filename, nickname, private_ip, access_key_id, secret_key, user_token, product_token, bucket_name)
