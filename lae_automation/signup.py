#!/usr/bin/python

import time
from base64 import b32encode

from twisted.internet import defer, reactor, task
from twisted.python.filepath import FilePath

from lae_automation.config import Config
from lae_automation.initialize import create_stripe_user_bucket, deploy_EC2_instance, verify_and_store_serverssh_pubkey
from lae_automation.aws.queryapi import TimeoutError, wait_for_EC2_addresses
from lae_automation.server import install_server, bounce_server, NotListeningError
from lae_automation.confirmation import send_signup_confirmation, send_notify_failure
from lae_util.servers import append_record
from lae_util.timestamp import format_iso_time

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

POLL_TIME = 30

# credit card verification might take 15 minutes, so wait 20.
CC_VERIFICATION_TIME = 20 * 60

# wait 75 seconds before the first poll, then up to 5 minutes for the addresses.
ADDRESS_DELAY_TIME = 75
ADDRESS_WAIT_TIME = 5 * 60

LISTEN_RETRIES = 5
LISTEN_POLL_TIME = 15
VERIFY_POLL_TIME = 5
VERIFY_TOTAL_WAIT = 600


def lookup_product(config, productcode):
    ps = [p for p in config.products if p['product_code'] == productcode]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (productcode, len(ps)))

    return ps[0]


def encode_id(ident):
    return b32encode(ident).rstrip("=").lower()

def get_bucket_name(subscription_id, customer_id):
    return "lae-%s-%s" % (encode_id(subscription_id), encode_id(customer_id))


def activate_subscribed_service(customer_email, customer_pgpinfo, customer_id, subscription_id,
                                plan_id, stdout, stderr, secretsfile, logfile,
                                configpath='../secret_config/lae_automation_config.json',
                                serverinfopath=None, clock=None):
    print >> stderr, "entering activate_subscribed_service call."
    print >> stderr, "configpath is %s" % configpath
    config = Config(configpath)
    myclock = clock or reactor
    AWSaccesskeyid = config.other["ec2_access_key_id"]
    AWSsecretkeypath = config.other["ec2_secret_path"]
    AWSsecretkey = FilePath(AWSsecretkeypath).getContent().strip()
    bucketname = get_bucket_name(subscription_id, customer_id)
    location = None  # default S3 location for now
    print >> stderr, "plan_id is %s" % plan_id
    product = lookup_product(config, plan_id)
    fullname = product['full_name']
    amiimageid = product['ami_image_id']
    instancesize = product['instance_size']

    print >>stdout, "Signing up customer for %s..." % (fullname,)

    d = create_stripe_user_bucket(AWSaccesskeyid, AWSsecretkey, bucketname, stdout, stderr, location)

    print >>stdout, "After create_stripe_account_user_bucket %s..." % str((AWSaccesskeyid, AWSsecretkey, bucketname,
                                                          None, amiimageid, instancesize,
                                                          customer_email,
                                                          customer_pgpinfo, stdout, stderr,
                                                          secretsfile, config, serverinfopath,
                                                          AWSsecretkeypath, myclock))
    # We could deploy and configure the instance in parallel with the above wait and delete it
    # if necessary, but let's keep it simple and sequential.
    d.addCallback(lambda ign: deploy_stripeaccount_server(AWSaccesskeyid, AWSsecretkey, bucketname,
                                                          None, amiimageid, instancesize,
                                                          customer_email,
                                                          customer_pgpinfo, stdout, stderr,
                                                          secretsfile, config, serverinfopath,
                                                          AWSsecretkeypath, clock=myclock))

    d.addErrback(lambda f: send_notify_failure(f, customer_email, logfile, stdout, stderr))
    return d

def replace_server(oldsecrets, amiimageid, instancesize, customer_email, stdout, stderr,
                   secretsfile, logfilename,
                   configpath='../secret_config/lae_automation_config.json',
                   serverinfopath=None, ec2secretpath=None, clock=None):
    config = Config(configpath)
    useraccesskeyid = oldsecrets['access_key_id']
    usersecretkey   = oldsecrets['secret_key']
    usertoken       = oldsecrets['user_token']
    producttoken    = oldsecrets['product_token']
    bucketname      = oldsecrets["bucket_name"]

    d = deploy_server(useraccesskeyid, usersecretkey, usertoken, producttoken,
                      bucketname, oldsecrets, amiimageid, instancesize,
                      "someone", customer_email, None, stdout, stderr,
                      secretsfile, config, serverinfopath, ec2secretpath, clock)
    d.addErrback(lambda f: send_notify_failure(f, customer_email, logfilename, stdout,
                                               stderr))
    return d

# TODO: too many args. Consider passing them in a dict.
def deploy_server(useraccesskeyid, usersecretkey, usertoken, producttoken,
                  bucketname, oldsecrets, amiimageid, instancesize,
                  customer_name, customer_email, customer_pgpinfo, stdout, stderr,
                  secretsfile, config, serverinfopath=None, ec2secretpath=None, clock=None):
    serverinfopath = serverinfopath or '../serverinfo.csv'
    ec2secretpath = ec2secretpath or '../secret_config/ec2secret'
    myclock = clock or reactor

    ec2accesskeyid = str(config.other['ec2_access_key_id'])
    ec2secretkey = FilePath(ec2secretpath).getContent().strip()

    instancename = customer_email  # need not be unique

    admin_keypair_name = str(config.other['admin_keypair_name'])
    admin_privkey_path = str(config.other['admin_privkey_path'])
    monitor_pubkey = FilePath(str(config.other['monitor_pubkey_path'])).getContent().strip()
    monitor_privkey_path = str(config.other['monitor_privkey_path'])
    #sinkname_suffix = str(config.other['sinkname_suffix'])

    # XXX Here's where we decide whether the new signup goes to a new EC2.
    d = deploy_EC2_instance(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, amiimageid,
                            instancesize, bucketname, admin_keypair_name, instancename,
                            stdout, stderr)

    def _deployed(instance):
        print >>stdout, "Waiting %d seconds for the server to be ready..." % (ADDRESS_DELAY_TIME,)
        d2 = task.deferLater(myclock, ADDRESS_DELAY_TIME, wait_for_EC2_addresses,
                             ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, stdout, stderr, POLL_TIME,
                             ADDRESS_WAIT_TIME, instance.instance_id)

        def _got_addresses(addresses):
            assert len(addresses) == 1, addresses
            (publichost, privatehost) = addresses[0]
            print >>stdout, "The server's public address is %r." % (publichost,)

            d3 = verify_and_store_serverssh_pubkey(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT,
                                                   publichost, VERIFY_POLL_TIME, VERIFY_TOTAL_WAIT,
                                                   stdout, stderr, instance.instance_id)

            def _got_sshfp(ignored):
                retries = LISTEN_RETRIES
                while True:
                    try:
                        install_server(publichost, admin_privkey_path, monitor_pubkey,
                                       monitor_privkey_path, stdout, stderr)
                        break
                    except NotListeningError:
                        retries -= 1
                        if retries <= 0:
                            print >>stdout, "Timed out waiting for EC2 instance to listen for ssh connections."
                            raise TimeoutError()
                        print >>stdout, "Waiting another %d seconds..." % (LISTEN_POLL_TIME)
                        time.sleep(LISTEN_POLL_TIME)
                        continue

                furl = bounce_server(publichost, admin_privkey_path, privatehost, useraccesskeyid,
                                     usersecretkey, usertoken, producttoken, bucketname, oldsecrets,
                                     stdout, stderr, secretsfile)

                # Disabled for now.
                #initialize_statmover_source(publichost, monitor_privkey_path, admin_privkey_path,
                #                            sinkname_suffix, [instance.instance_id, 'SSEC2s'])

                # XXX We probably need to rethink this:
                append_record(FilePath(serverinfopath), instance.launch_time, instance.instance_id,
                              publichost)

                print >>stderr, "Signup done."
                d4 = defer.succeed(None)
                if not oldsecrets:
                    d4.addCallback(lambda ign: send_signup_confirmation(publichost, customer_name,
                                                                        customer_email, furl,
                                                                        customer_pgpinfo,
                                                                        stdout, stderr) )
                return d4
            d3.addCallback(_got_sshfp)
            return d3
        d2.addCallback(_got_addresses)
        return d2
    d.addCallback(_deployed)
    return d

# TODO: too many args. Consider passing them in a dict.
def deploy_stripeaccount_server(AWSaccesskeyid, AWSsecretkey, bucketname, oldsecrets, amiimageid,
                                instancesize, customer_email, customer_pgpinfo,
                                stdout, stderr, secretsfile, config, serverinfopath,
                                ec2secretpath, clock):
    print >> stdout, "inside deploy_stripeaccount_server"
    serverinfopath = serverinfopath or '../serverinfo.csv'
    ec2secretpath = ec2secretpath or '../secret_config/ec2secret'
    myclock = clock or reactor

    ec2accesskeyid = str(config.other['ec2_access_key_id'])
    ec2secretkey = FilePath(ec2secretpath).getContent().strip()

    instancename = customer_email  # need not be unique

    admin_keypair_name = str(config.other['admin_keypair_name'])
    admin_privkey_path = str(config.other['admin_privkey_path'])
    monitor_pubkey = FilePath(str(config.other['monitor_pubkey_path'])).getContent().strip()
    monitor_privkey_path = str(config.other['monitor_privkey_path'])
    #sinkname_suffix = str(config.other['sinkname_suffix'])

    # XXX Here's where we decide whether the new signup goes to a new EC2.
    d = deploy_EC2_instance(AWSaccesskeyid, AWSsecretkey, EC2_ENDPOINT, amiimageid,
                            instancesize, bucketname, admin_keypair_name, instancename,
                            stdout, stderr)

    def _deployed(instance):
        print >>stdout, "Waiting %d seconds for the server to be ready..." % (ADDRESS_DELAY_TIME,)
        d2 = task.deferLater(myclock, ADDRESS_DELAY_TIME, wait_for_EC2_addresses,
                             ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, stdout, stderr, POLL_TIME,
                             ADDRESS_WAIT_TIME, instance.instance_id)

        def _got_addresses(addresses):
            assert len(addresses) == 1, addresses
            (publichost, privatehost) = addresses[0]
            print >>stdout, "The server's public address is %r." % (publichost,)

            d3 = verify_and_store_serverssh_pubkey(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT,
                                                   publichost, VERIFY_POLL_TIME, VERIFY_TOTAL_WAIT,
                                                   stdout, stderr, instance.instance_id)

            def _got_sshfp(ignored):
                retries = LISTEN_RETRIES
                while True:
                    try:
                        install_server(publichost, admin_privkey_path, monitor_pubkey,
                                       monitor_privkey_path, stdout, stderr)
                        break
                    except NotListeningError:
                        retries -= 1
                        if retries <= 0:
                            print >>stdout, "Timed out waiting for EC2 instance to listen for ssh connections."
                            raise TimeoutError()
                        print >>stdout, "Waiting another %d seconds..." % (LISTEN_POLL_TIME)
                        time.sleep(LISTEN_POLL_TIME)
                        continue

                furl = bounce_server(publichost, admin_privkey_path, privatehost, AWSaccesskeyid,
                                     AWSsecretkey, bucketname, oldsecrets, stdout, stderr, secretsfile)

                # Disabled for now.
                #initialize_statmover_source(publichost, monitor_privkey_path, admin_privkey_path,
                #                            sinkname_suffix, [instance.instance_id, 'SSEC2s'])

                # XXX We probably need to rethink this:
                append_record(FilePath(serverinfopath), instance.launch_time, instance.instance_id,
                              publichost)

                print >>stderr, "Signup done."
                d4 = defer.succeed(None)
                if not oldsecrets:
                    d4.addCallback(lambda ign: send_signup_confirmation(publichost, 
                                                                        customer_email, furl,
                                                                        customer_pgpinfo,
                                                                        stdout, stderr) )
                return d4
            d3.addCallback(_got_sshfp)
            return d3
        d2.addCallback(_got_addresses)
        return d2
    d.addCallback(_deployed)
    return d


def create_log_filepaths(stripe_plan_id, stripe_customer_id, stripe_subscription_id):
    logdir_parent_fp = FilePath('../').child('secrets').child(stripe_plan_id)
    timestamp = format_iso_time(time.time())
    fpcleantimestamp = timestamp.replace(':', '')
    logdirname = "%s-%s" % (fpcleantimestamp, get_bucket_name(stripe_customer_id, stripe_subscription_id)[len('lae-') :])
    abslogdir_fp = logdir_parent_fp.child(logdirname)
    abslogdir_fp.makedirs()
    stripelog_fp = abslogdir_fp.child('stripe')
    SSEC2log_fp = abslogdir_fp.child('SSEC2')
    signuplog_fp = abslogdir_fp.child('signup_logs')
    return abslogdir_fp, stripelog_fp, SSEC2log_fp, signuplog_fp
