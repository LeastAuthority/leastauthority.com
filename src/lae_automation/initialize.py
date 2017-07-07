# Copyright Least Authority Enterprises.
# See LICENSE for details.

from eliot import startAction
from eliot.twisted import DeferredContext

from lae_util import retry_failure, backoff

from txaws.s3.exception import S3Error

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
