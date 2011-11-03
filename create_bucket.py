import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials
from lae_site.user.initialize import create_user_bucket

if len(sys.argv) < 6:
    print "Usage: python create_bucket.py ACCESS_KEY_ID SECRET_KEY USER_TOKEN PRODUCT_TOKEN BUCKET_NAME [LOCATION]"
    print "Happy bucket-creating!"
    sys.exit(1)

# TODO: the access key, secret key, user token and product token should probably be in files.
accesskeyid = sys.argv[1]
secretkey = sys.argv[2]
usertoken = sys.argv[3]
producttoken = sys.argv[4]
bucketname = sys.argv[5]
if len(sys.argv) > 6:
    location = sys.argv[6]
else:
    location = None  # default

creds = AWSCredentials(accesskeyid, secretkey)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = create_user_bucket(creds, usertoken, bucketname, cb, producttoken=producttoken, location=location)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
