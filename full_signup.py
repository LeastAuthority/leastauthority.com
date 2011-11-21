
import sys
from twisted.internet import defer, reactor, task
from twisted.python.failure import Failure
from twisted.python.filepath import FilePath
from txaws.service import AWSCredentials

from lae_site.config import Config
from lae_site.user.initialize import activate_user_account_desktop, verify_user_account, \
    create_user_bucket, deploy_EC2_instance, get_EC2_addresses
from lae_site.user.configure import install_server, bounce_server


config = Config()

class TimeoutError:
    pass

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'


def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response


def signup(activationkey, productcode, bucketname, location=None, clock=None):
    ps = [p for p in config.products if p['product_code'] == productcode]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (productcode, len(ps)))

    product = ps[0]
    fullname = product['full_name']
    producttoken = product['product_token']
    amiimageid = product['ami_image_id']
    instancesize = product['instance_size']

    ec2accesskeyid = config.other['ec2_access_key_id']
    ec2secretkey = FilePath('../ec2secret').getContent()
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)

    ec2keypairname = config.other['keypair_name']
    ec2keyfilename = config.other['key_filename']

    print "Signing up user for %s..." % (fullname,)

    d = activate_user_account_desktop(activationkey, producttoken, cb)
    def _activated( (usercreds, usertoken) ):
        def _wait_until_verified(how_long_secs):
            d3 = verify_user_account(usercreds, usertoken, producttoken, cb)
            def _maybe_again(res):
                if res:
                    return
                if how_long_secs <= 0.0:
                    raise TimeoutError("timed out waiting for verification of subscription")
                myclock = clock or reactor
                return task.deferLater(myclock, 30.0, _wait_until_verified, how_long_secs - 30.0, myclock)
            d3.addCallback(_maybe_again)
            return d3

        # credit card verification might take 15 minutes, so wait 20.
        d2 = _wait_until_verified(20 * 60.0)

        d2.addCallback(lambda ign: create_user_bucket(usercreds, usertoken, bucketname, cb, producttoken=producttoken, location=location))

        # We could deploy and configure the instance in parallel with the above wait and delete it
        # if necessary, but let's keep it simple and sequential.
        d2.addCallback(lambda ign: deploy_EC2_instance(ec2creds, EC2_ENDPOINT, amiimageid, instancesize,
                                                       bucketname, ec2keypairname, False, cb))

        def _deployed(instance):
            def _wait_for_addresses(how_long_secs):
                d4 = get_EC2_addresses(instance, cb)
                def _maybe_again2(res):
                    if res:
                        return res
                    if how_long_secs <= 0.0:
                        raise TimeoutError("timed out waiting for addresses of EC2 instance")
                    myclock = clock or reactor
                    return task.deferLater(myclock, 30.0, _wait_until_verified, how_long_secs - 30.0, myclock)
                d4.addCallback(_maybe_again2)
                return d4

            # wait 5 minutes for the addresses
            d3 = _wait_for_addresses(5 * 60.0)
            def _got_addresses( (publicip, privateip) ):
                d4 = defer.succeed(None)
                d4.addCallback(lambda ign: install_server(publicip, ec2keyfilename))
                d4.addCallback(lambda ign: bounce_server(publicip, ec2keyfilename, privateip,
                                                         usercreds, usertoken, producttoken, bucketname))
                return d4
            d3.addCallback(_got_addresses)
            return d3
        d2.addCallback(_deployed)
        return d2
    d.addCallback(_activated)
    return d


def main(argv):
    if len(argv) < 4:
        print "Usage: python full_signup.py ACTIVATION_KEY PRODUCT_CODE BUCKET_NAME [LOCATION]"
        return 1

    activationkey = sys.argv[1]
    productcode = sys.argv[2]
    bucketname = sys.argv[3]
    if len(sys.argv) > 4:
        location = sys.argv[4]
    else:
        location = None  # default

    return signup(activationkey, productcode, bucketname, location=location)


if __file__ == '__main__':
    d = defer.succeed(sys.argv)
    d.addCallback(main)
    d.addBoth(cb)
    d.addBoth(lambda ign: reactor.stop())
    reactor.run()
