import sys
from twisted.internet import reactor
from twisted.python.failure import Failure
from txaws.service import AWSCredentials
from lae_site.user.initialize import deploy_EC2_instance


if len(sys.argv) < 7:
    print "Usage: python deploy.py ACCESS_KEY_ID SECRET_KEY AMI_IMAGE_ID INSTANCE_SIZE CUSTOMER_EMAIL KEYPAIR_NAME"
    print "Happy deploying!"
    sys.exit(1)

access_key_id = sys.argv[1]
secret_key = sys.argv[2]
ami_image_id = sys.argv[3]
instance_size = sys.argv[4]
customer_email = sys.argv[5]
keypair_name = sys.argv[6]
creds = AWSCredentials(access_key_id, secret_key)

EC2_ENDPOINT = 'https://ec2.us-east-1.amazonaws.com/'
#EC2_ENDPOINT = 'https://ec2.amazonaws.com/'

def cb(x):
    print str(x)
    if isinstance(x, Failure) and hasattr(x.value, 'response'):
        print x.value.response

d = deploy_EC2_instance(creds, EC2_ENDPOINT, ami_image_id, instance_size, customer_email, keypair_name, cb)
d.addBoth(cb)
d.addBoth(lambda ign: reactor.stop())
reactor.run()
