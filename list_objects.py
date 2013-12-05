from lae_automation.aws.devpay_s3client import DevPayS3Client

from txaws.service import AWSCredentials, AWSServiceEndpoint

from txaws import version

#Build the credential
access_key_id = XXX
access_secret_key = XXX 
access_credential = AWSCredentials(access_key=access_key_id, secret_key=access_secret_key)
producttoken = XXX
usertoken = XXX

#Name the bucket
bucketname = XXX
#Build the endpoint
aws_endpoint = AWSServiceEndpoint(uri='http://s3.amazonaws.com')
#Build a client
listing_client = DevPayS3Client(access_credential, usertoken, producttoken=producttoken, endpoint=aws_endpoint)

#Make the request
d = listing_client.get_bucket(bucketname)

def printout_fail(result):
    print result
    reactor.stop()

def print_objects(result):
    print result.contents
    reactor.stop()

d.addCallback(print_objects)
d.addErrback(printout_fail)
print access_key_id
print access_secret_key
print len(access_secret_key)
print aws_endpoint.get_uri()
print version

from twisted.internet import reactor
reactor.run()
