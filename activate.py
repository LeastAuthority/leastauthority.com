import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials
from lae_site.user.initialize import initialize_user_account

if len(sys.argv) < 5:
    print "Usage: python activate.py ACCESS_KEY SECRET_KEY LONG_PRODUCT_TOKEN ACTIVATION_KEY"
    print "Happy activating!"
    sys.exit(1)

access_key = sys.argv[1]
secret_key = sys.argv[2]
product_token = sys.argv[3]
activation_key = sys.argv[4]
creds = AWSCredentials(access_key, secret_key)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = initialize_user_account(creds, activation_key, product_token, cb)
d.addBoth(cb)
d.addCallback(lambda ign: reactor.stop())
reactor.run()
