"""
-- The developers public keys, corresponding to which ever private keys they specify will live on the web server.  
-- These are transferred to the new EC2 instance in /home/customer/.ssh, and /home/ubuntu/.ssh
"""

import sys, time
from fabric import operations
from fabric.api import local, sudo, env, reboot, run
from cStringIO import StringIO

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

def set_ip_and_key(public_ip, key_filename):
    env.host_string = 'ubuntu@' + public_ip
    env.reject_unknown_hosts = False  # FIXME allows MITM attacks
    local("chmod go-r %s" % key_filename)
    env.key_filename = key_filename

def sudo_apt_get(argstring):
    sudo('apt-get %s' % argstring)

def sudo_easy_install(argstring):
    sudo('easy_install %s' % argstring)

def as_customer(argstring):
    sudo(argstring, user='customer')

def write_as_customer(remote_path, value, mode=None):
    # remote_path must not require quoting
    r = operations.put(StringIO(value), remote_path, mode=mode, use_sudo=True)
    return str(r) + '\n' + sudo('chown customer:customer ' + remote_path)

def read_as_customer(remote_path):
    # remote_path must not require quoting
    return sudo('cat ' + remote_path, user='customer')

def delete_customer(public_ip, key_filename):
    set_ip_and_key(public_ip, key_filename)
    print sudo('deluser customer')
    print sudo('rm -rf /home/customer*')

def install_server(public_ip, key_filename):
    set_ip_and_key(public_ip, key_filename)
    print sudo_apt_get('update')
    print sudo_apt_get('upgrade -y')
    print sudo_apt_get('install -y linux-ec2 linux-image-ec2')
    reboot(60)
    print sudo_apt_get('install -y python-dev')
    print sudo_apt_get('install -y python-setuptools')
    print sudo_apt_get('install -y exim4-base')
    print sudo_apt_get('install -y darcs')
    print sudo_easy_install('foolscap')
    print run('wget https://leastauthority.com/content/static/patches/txAWS-0.2.1.post2.tar.gz')
    print run('tar -xzvf txAWS-0.2.1.post2.tar.gz')
    print sudo("/bin/sh -c 'cd /home/ubuntu/txAWS-0.2.1.post2 && python ./setup.py install'")
    print sudo('adduser --disabled-password --gecos "" customer || echo Assuming that user already exists.')
    print sudo('mkdir -p /home/customer/.ssh/')
    print sudo('chown customer:customer /home/customer/.ssh')
    print sudo('cp /home/ubuntu/.ssh/authorized_keys /home/customer/.ssh/authorized_keys')
    print sudo('chown customer:customer /home/customer/.ssh/authorized_keys')
    print sudo('chmod 400 /home/customer/.ssh/authorized_keys')
    print sudo('chmod 700 /home/customer/.ssh/')
    print as_customer('whoami')
    print as_customer('rm -rf /home/customer/LAFS_source')
    print as_customer("/bin/sh -c 'cd /home/customer && darcs get --lazy https://tahoe-lafs.org/source/tahoe/ticket999-S3-backend LAFS_source'")
    print as_customer("/bin/sh -c 'cd /home/customer/LAFS_source && python ./setup.py build'")
    print as_customer('mkdir -p /home/customer/introducer /home/customer/storageserver')
    print as_customer("/bin/sh -c 'cd /home/customer/LAFS_source && ./bin/tahoe create-introducer ../introducer || echo Assuming that introducer already exists.'")
    print as_customer("/bin/sh -c 'cd /home/customer/LAFS_source && ./bin/tahoe create-node ../storageserver || echo Assuming that storage server already exists.'")

def bounce_server(public_ip, key_filename, nickname, private_ip, access_key_id, secret_key, user_token, product_token, bucket_name):
    set_ip_and_key(public_ip, key_filename)

    print write_as_customer('/home/customer/introducer/introducer.port', '12345\n')
    print write_as_customer('/home/customer/storageserver/client.port', '12346\n')
    print as_customer("/bin/sh -c 'cd /home/customer/LAFS_source && ./bin/tahoe restart ../introducer'")
    time.sleep(2.0)
    introducer_furl = read_as_customer('/home/customer/introducer/introducer.furl')

    tahoe_cfg = TAHOE_CFG_TEMPLATE % {'nickname': nickname,
                                      'public_ip': public_ip,
                                      'private_ip': private_ip,
                                      'introducer_furl': introducer_furl,
                                      'access_key_id': access_key_id,
                                      'bucket_name': bucket_name}
    print write_as_customer('/home/customer/storageserver/tahoe.cfg', tahoe_cfg)
    print write_as_customer('/home/customer/storageserver/private/s3secret', secret_key, mode=0440)
    print write_as_customer('/home/customer/storageserver/private/s3usertoken', user_token, mode=0440)
    print write_as_customer('/home/customer/storageserver/private/s3producttoken', product_token, mode=0440)

    print as_customer("/bin/sh -c 'cd /home/customer/LAFS_source && ./bin/tahoe restart ../storageserver'")


if len(sys.argv) < 10:
    print "Usage: python configure.py PUBLIC_IP KEY_FILE NICKNAME PRIVATE_IP ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN BUCKET_NAME"
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

install_server(public_ip, key_filename)
bounce_server(public_ip, key_filename, nickname, private_ip, access_key_id, secret_key, user_token, product_token, bucket_name)
