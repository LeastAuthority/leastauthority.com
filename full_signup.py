#!/usr/bin/python

import sys, time
from twisted.internet import defer, reactor, task
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath
from txaws.service import AWSCredentials

from lae_site.config import Config
from lae_site.user.initialize import activate_user_account_desktop, verify_user_account, \
    create_user_bucket, deploy_EC2_instance, get_EC2_addresses
from lae_site.user.configure import install_server, bounce_server, NotListeningError


config = Config()

class TimeoutError:
    pass

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

POLL_TIME = 30


def signup(name, email, activationkey, productcode, keyinfo, stdout, stderr, clock=None):
    def cb(x):
        print >>stdout, str(x)
        if isinstance(x, Failure) and hasattr(x.value, 'response'):
            print >>stdout, x.value.response

    myclock = clock or reactor
    bucketname = "lae-%s-%s" % (productcode.lower(), activationkey.lower())
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
    print "%r %r" % (ec2accesskeyid, ec2secretkey)
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)

    ec2keypairname = str(config.other['keypair_name'])
    ec2keyfilename = str(config.other['key_filename'])

    print >>stdout, "Signing up user for %s..." % (fullname,)

    d = activate_user_account_desktop(activationkey, producttoken, cb)
    def _activated(adpr):
        usercreds = AWSCredentials(adpr.access_key_id, adpr.secret_key)
        usertoken = adpr.usertoken

        def _wait_until_verified(how_long_secs):
            d3 = verify_user_account(usercreds, usertoken, producttoken, cb)
            def _maybe_again(res):
                if res:
                    return
                if how_long_secs <= 0.0:
                    raise TimeoutError("timed out waiting for verification of subscription")
                    cb("Waiting another %d seconds..." % (POLL_TIME,))
                    return task.deferLater(myclock, POLL_TIME, _wait_until_verified, how_long_secs - POLL_TIME)
            d3.addCallback(_maybe_again)
            return d3

        # credit card verification might take 15 minutes, so wait 20.
        d2 = _wait_until_verified(20 * 60.0)

        d2.addCallback(lambda ign: create_user_bucket(usercreds, usertoken, bucketname, cb, producttoken=producttoken, location=location))

        # We could deploy and configure the instance in parallel with the above wait and delete it
        # if necessary, but let's keep it simple and sequential.
        d2.addCallback(lambda ign: deploy_EC2_instance(ec2creds, EC2_ENDPOINT, amiimageid, instancesize,
                                                       bucketname, ec2keypairname, cb))

        def _deployed(instance):
            def _wait_for_addresses(how_long_secs):
                d4 = get_EC2_addresses(ec2creds, EC2_ENDPOINT, instance.instance_id)
                def _maybe_again2(res):
                    if res:
                        return res
                    if how_long_secs <= 0.0:
                        raise TimeoutError("timed out waiting for addresses of EC2 instance")
                    cb("Waiting another %d seconds..." % (POLL_TIME,))
                    return task.deferLater(myclock, POLL_TIME, _wait_for_addresses, how_long_secs - POLL_TIME)
                d4.addCallback(_maybe_again2)
                return d4

            # wait 10 seconds before the first poll, then up to 5 minutes for the addresses
            d3 = task.deferLater(myclock, 10, _wait_for_addresses, 5 * 60.0)

            def _got_addresses( (publichost, privatehost) ):
                cb("publichost = %r, privatehost = %r" % (publichost, privatehost))
                while True:
                    try:
                        install_server(publichost, ec2keyfilename)
                        break
                    except NotListeningError:
                        cb("Waiting another 10 seconds...")
                        time.sleep(10)
                        continue

                bounce_server(publichost, ec2keyfilename, privatehost,
                              usercreds, usertoken, producttoken, bucketname)
            d3.addCallback(_got_addresses)
            return d3
        d2.addCallback(_deployed)
        return d2
    d.addCallback(_activated)
    return d


def main(stdin, stdout, stderr):
    print >>stderr, "On separate lines: Name, Email, Activation key, Product code, Key info"
    name = stdin.readline()
    email = stdin.readline()
    activationkey = stdin.readline()
    productcode = stdin.readline()
    keyinfo = stdin.readline()

    if keyinfo is None:
        # EOF reached before 5 lines (including blank lines) were input
        print >>stdout, "full_signup.py: some information was not received. Please report this to <info@leastauthority.com>."
        return 1

    return signup(name, email, activationkey, productcode, keyinfo, stdout, stderr)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = defer.succeed(None)
d.addCallback(lambda ign: main(sys.stdin, sys.stdout, sys.stderr))
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
