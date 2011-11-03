import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials
from lae_site.user.initialize import create_user_bucket

if len(sys.argv) < 6:
    print "Usage: python create_bucket.py ACCESS_KEY_ID SECRET_KEY USER_TOKEN PRODUCT_TOKEN BUCKET_NAME"
    print "Happy bucket-creating!"
    sys.exit(1)

# TODO: the access key, secret key, user token and product token should probably be in files.
accesskeyid = sys.argv[1]
secretkey = sys.argv[2]
usertoken = sys.argv[3]
producttoken = sys.argv[4]
bucketname = sys.argv[5]
creds = AWSCredentials(accesskeyid, secretkey)

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = create_user_bucket(creds, usertoken, bucketname, cb, producttoken=producttoken)
d.addBoth(cb)
d.addCallback(lambda ign: reactor.stop())
reactor.run()
