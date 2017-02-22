# Copyright Least Authority Enterprises.
# See LICENSE for details.

from eliot import startAction
from eliot.twisted import DeferredContext

from lae_util import retry_failure, backoff

from txaws.s3.exception import S3Error
from txaws.service import AWSServiceRegion
from txaws.credentials import AWSCredentials

def create_user_bucket(reactor, client, bucketname):
    """
    Create an S3 bucket for a user's grid.

    If S3 errors are encountered, retries will be attempted.  If too
    many errors are encountered, this will give up and return a
    failure.

    @param reactor: An IReactorTime which can be used to schedule retries.
    @param client: A txaws.s3.client.S3Client which can be used to create the bucket.
    @param bucketname: The name of the S3 bucket to create.

    @return: A Deferred that fires when the bucket has been created or
        fails when too many errors are encountered.
    """
    action = startAction(
        action_type=u"initialize:create_user_bucket",
        name=bucketname,
    )
    with action.context():
        d = DeferredContext(
            retry_failure(
                reactor,
                lambda: client.create_bucket(bucketname),
                expected=[S3Error],
                steps=backoff(),
            ),
        )
        return d.addActionFinish()


def create_stripe_user_bucket(accesskeyid, secretkey, bucketname, stdout, stderr, location, reactor=None):
    if reactor is None:
        from twisted.internet import reactor

    assert location is None, "Alternate S3 bucket locations unimplemented."

    print >>stderr, ('usertoken = %r\n'
                     'bucketname = %r\n'
                     'location = %r\n'
                     'accesskeyid = %r\n'
                     'secretkey = %r\n'
                     % (None, bucketname, location, accesskeyid, secretkey))

    region = AWSServiceRegion(creds=AWSCredentials(
        accesskeyid.encode("ascii"),
        secretkey.encode("ascii"),
    ))
    client = region.get_s3_client()
    print >>stderr, "client is %s" % (client,)

    d  = create_user_bucket(reactor, client, bucketname)

    def bucket_created(res):
        print >>stdout, "S3 bucket created."
        print >>stderr, repr(res)

    def bucket_creation_failed(res):
        print >>stderr, "S3 bucket creation failed."
        print >>stderr, repr(res)
        print >>stderr, repr(res.value)
        print >>stderr, repr(res.printTraceback())
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx
        # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        # exception swallowed, so bucket creation failure is never ever noticed
    d.addCallbacks(bucket_created, bucket_creation_failed)
    return d
