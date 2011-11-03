import logging

from lae_site.aws.license_service_client import LicenseServiceClient
from lae_site.aws.devpay_s3client import DevPayS3Client


def activate_user_account_desktop(activationkey, producttoken, status_callback):
    """
    @param activationkey:
            The activationkey sent from the users browser upon completion
            of the DevPay signup process.

    @param status_callback(status):
            A function which will be called multiple times with a status
            string for user feedback.

    @return:
            A Deferred which fires with an ActivateDesktopProductResponse upon
            successful user initialization.
    """

    log = logging.getLogger('activate_user_account')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Activating DevPay License for decentralized ("desktop") product...',
        activationkey = activationkey)

    d = LicenseServiceClient().activate_desktop_product(activationkey, producttoken)

    def activated(adpr):
        update_status(
            public = ('DevPay License activated:\n'
                      'access_key_id=%s\n'
                      'secret_key=%s\n'
                      'usertoken=%s\n' % (adpr.access_key_id, adpr.secret_key, adpr.usertoken)),
            activation_response = adpr)
        return adpr
    d.addCallback(activated)
    return d


def activate_user_account_hosted(creds, activationkey, producttoken, status_callback):
    """
    @param creds: AWSCredentials

    @param activationkey:
            The activationkey sent from the users browser upon completion
            of the DevPay signup process.

    @param status_callback(status):
            A function which will be called multiple times with a status
            string for user feedback.

    @return:
            A Deferred which fires with an ActivateHostedProductResponse upon
            successful user initialization.
    """

    log = logging.getLogger('activate_user_account')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Activating DevPay License for centralized ("hosted") product...',
        activationkey = activationkey)

    d = LicenseServiceClient(creds).activate_hosted_product(activationkey, producttoken)

    def activated(ahpr):
        update_status(
            public = ('DevPay License activated:\n'
                      'usertoken=%s\n'
                      'pid=%s\n' % (ahpr.usertoken, ahpr.pid)),
            activation_response = ahpr)
        return ahpr
    d.addCallback(activated)
    return d


def create_user_bucket(creds, usertoken, bucketname, status_callback, producttoken=None):
    log = logging.getLogger('create_user_bucket')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Creating S3 Bucket...',
        usertoken = usertoken,
        bucketname = bucketname)

    s3c = DevPayS3Client(
        creds = creds,
        usertoken = usertoken,
        producttoken = producttoken)

    d = s3c.create_bucket(str(bucketname))

    def bucket_created(*args, **kw):
        update_status(
            public = 'S3 Bucket created.',
            UNKNOWNS = (args, kw))

    d.addCallback(bucket_created)
    return d
