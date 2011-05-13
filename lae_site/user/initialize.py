import logging

from lae_site.aws.license_service_client import LicenseServiceClient
from lae_site.aws.devpay_s3client import DevPayS3Client
from lae_site.util.entropy import EntropicToken


def initialize_user_account(creds, activationkey, status_callback):
    """
    @param creds: AWSCredentials

    @param activationkey:
            The activationkey sent from the users browser upon completion
            of the DevPay signup process.

    @param status_callback(status):
            A function which will be called multiple times with a status
            string for user feedback.

    @return:
            A Deferred which fires with None upon successful user
            initialization.
    """

    log = logging.getLogger('initialize_user_account')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Activating DevPay License...',
        activationkey = activationkey)

    d = LicenseServiceClient(creds).activate_hosted_product(activationkey)

    def activated(ahpr):
        update_status(
            public = 'DevPay License activated.',
            activation_response = ahpr)

        bucketname = EntropicToken.generate()
        update_status(
            public = 'Creating S3 Bucket...',
            bucketname = bucketname)

        s3c = DevPayS3Client(
            creds = creds,
            devpayusertoken = ahpr.usertoken)

        return s3c.create_bucket(str(bucketname))

    d.addCallback(activated)

    def bucket_created(*args, **kw):
        update_status(
            public = 'S3 Bucket created.',
            UNKNOWNS = (args, kw))

        return None

    d.addCallback(bucket_created)

    return d
