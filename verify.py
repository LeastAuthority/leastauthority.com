import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials
from lae_site.user.initialize import verify_user_account

if len(sys.argv) < 5:
    print "Usage: python verify.py ACCESS_KEY_ID SECRET_KEY USER_TOKEN LONG_PRODUCT_TOKEN"
    print "Trust, but verify!"
    sys.exit(1)

# TODO: these tokens should probably be in files.
access_key_id = sys.argv[1]
secret_key = sys.argv[2]
user_token = sys.argv[3]
product_token = sys.argv[4]
creds = AWSCredentials(access_key_id, secret_key)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = verify_user_account(creds, user_token, product_token, cb)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
