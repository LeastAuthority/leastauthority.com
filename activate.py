import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
#from txaws.service import AWSCredentials
from lae_site.user.initialize import activate_user_account_desktop
#XXX  from lae_site.user.initialize import deploy_EC2_server <-- probably in module called deploy.py

#if len(sys.argv) < 5:
#    print "Usage: python activate.py ACCESS_KEY_ID SECRET_KEY ACTIVATION_KEY LONG_PRODUCT_TOKEN"
#    print "Happy activating!"
#    sys.exit(1)

if len(sys.argv) < 3:
    print "Usage: python activate.py ACTIVATION_KEY LONG_PRODUCT_TOKEN"
    print "Happy activating!"
    sys.exit(1)

# TODO: the product token should probably be in a file.
#access_key_id = sys.argv[1]
#secret_key = sys.argv[2]
activation_key = sys.argv[1]
product_token = sys.argv[2]
#creds = AWSCredentials(access_key_id, secret_key)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = activate_user_account_desktop(activation_key, product_token, cb)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
