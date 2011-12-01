#!/usr/bin/python

import time
from twisted.internet import reactor, task
from twisted.python.filepath import FilePath
from txaws.service import AWSCredentials

from lae_automation.config import Config
from lae_automation.initialize import activate_user_account_desktop, verify_user_account, \
    create_user_bucket, deploy_EC2_instance, get_EC2_addresses
from lae_automation.server import install_server, bounce_server, NotListeningError


config = Config()

class TimeoutError:
    pass

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

POLL_TIME = 30


def signup(activationkey, productcode, name, email, keyinfo, stdout, stderr, seed, secretsfile, clock=None):
    myclock = clock or reactor

    bucketname = "lae-%s-%s" % (productcode.lower(), seed)
    location = None  # default location for now

    ps = [p for p in config.products if p['product_code'] == productcode]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (productcode, len(ps)))

    product = ps[0]
    fullname = product['full_name']
    producttoken = product['product_token']
    amiimageid = product['ami_image_id']
    instancesize = product['instance_size']

    ec2accesskeyid = str(config.other['ec2_access_key_id'])
    ec2secretkey = FilePath('../ec2secret').getContent().strip()
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)

    ec2keypairname = str(config.other['keypair_name'])
    ec2keyfilename = str(config.other['key_filename'])

    print >>stdout, "Signing up customer for %s..." % (fullname,)

    d = activate_user_account_desktop(activationkey, producttoken, stdout, stderr)
    def _activated(adpr):
        usercreds = AWSCredentials(adpr.access_key_id, adpr.secret_key)
        usertoken = adpr.usertoken

        def _wait_until_verified(how_long_secs):
            d3 = verify_user_account(usercreds, usertoken, producttoken, stdout, stderr)
            def _maybe_again(res):
                if res:
                    print >>stdout, "Subscription verified."
                    return
                if how_long_secs <= 0.0:
                    raise TimeoutError("Timed out waiting for verification of subscription.")
                    print >>stdout, "Waiting another %d seconds..." % (POLL_TIME,)
                    return task.deferLater(myclock, POLL_TIME, _wait_until_verified, how_long_secs - POLL_TIME)
            d3.addCallback(_maybe_again)
            return d3

        # credit card verification might take 15 minutes, so wait 20.
        d2 = _wait_until_verified(20 * 60.0)

        d2.addCallback(lambda ign: create_user_bucket(usercreds, usertoken, bucketname, stdout, stderr,
                                                      producttoken=producttoken, location=location))

        # We could deploy and configure the instance in parallel with the above wait and delete it
        # if necessary, but let's keep it simple and sequential.
        d2.addCallback(lambda ign: deploy_EC2_instance(ec2creds, EC2_ENDPOINT, amiimageid, instancesize,
                                                       bucketname, ec2keypairname, stdout, stderr))

        def _deployed(instance):
            def _wait_for_addresses(how_long_secs):
                d4 = get_EC2_addresses(ec2creds, EC2_ENDPOINT, instance.instance_id)
                def _maybe_again2(res):
                    if res:
                        return res
                    if how_long_secs <= 0.0:
                        raise TimeoutError("Timed out waiting for addresses of EC2 instance.")
                    print >>stdout, "Waiting another %d seconds..." % (POLL_TIME,)
                    return task.deferLater(myclock, POLL_TIME, _wait_for_addresses, how_long_secs - POLL_TIME)
                d4.addCallback(_maybe_again2)
                return d4

            # wait 10 seconds before the first poll, then up to 5 minutes for the addresses
            d3 = task.deferLater(myclock, 10, _wait_for_addresses, 5 * 60.0)

            def _got_addresses( (publichost, privatehost) ):
                print >>stdout, "The server's public address is %r." % (publichost,)
                while True:
                    try:
                        install_server(publichost, ec2keyfilename, stdout, stderr)
                        break
                    except NotListeningError:
                        print >>stdout, "Waiting another 10 seconds..."
                        time.sleep(10)
                        continue

                bounce_server(publichost, ec2keyfilename, privatehost, usercreds, usertoken,
                              producttoken, bucketname, stdout, stderr, secretsfile)
            d3.addCallback(_got_addresses)
            return d3
        d2.addCallback(_deployed)
        return d2
    d.addCallback(_activated)
    return d
