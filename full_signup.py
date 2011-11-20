import sys
from twisted.internet import defer, reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials

from lae_site.config import Config

config = Config()


if len(sys.argv) < 4:
    print "Usage: python full_signup.py ACTIVATION_KEY PRODUCT_CODE CUSTOMER_EMAIL"
    sys.exit(1)

def signup(activation_key, product_code, customer_email):
    ps = [p for p in config.products if p['product_code'] == product_code]
    if len(ps) != 1:
        raise AssertionError("Product code %r matches %d products." % (product_code, len(ps)))

    product = ps[0]
    full_name = product['full_name']
    product_token = product['product_token']
    ami_image_id = product['ami_image_id']
    instance_size = product['instance_size']

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = defer.succeed(None)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
