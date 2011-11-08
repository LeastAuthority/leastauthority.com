import logging, urllib

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

    log = logging.getLogger('activate_user_account_desktop')

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

    log = logging.getLogger('activate_user_account_hosted')

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


def create_user_bucket(creds, usertoken, bucketname, status_callback, producttoken=None, location=None):
    log = logging.getLogger('create_user_bucket')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Creating S3 Bucket...',
        usertoken = usertoken,
        producttoken = producttoken,
        bucketname = bucketname,
        location = location)

    client = DevPayS3Client(
        creds = creds,
        usertoken = usertoken,
        producttoken = producttoken)

    if location:
        object_name = "?LocationConstraint=" + urllib.quote(location)
    else:
        object_name = None

    query = client.query_factory(
        action="PUT", creds=client.creds, endpoint=client.endpoint,
        bucket=bucketname, object_name=object_name)
    d = query.submit()

    def bucket_created(*args, **kw):
        update_status(
            public = 'S3 Bucket created.',
            UNKNOWNS = (args, kw))

    d.addCallback(bucket_created)
    return d


def verify_user_account(creds, usertoken, producttoken, status_callback):
    log = logging.getLogger('verify_user_account')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    update_status(
        public = 'Verifying DevPay License for decentralized ("desktop") product...',
        usertoken = usertoken,
        producttoken = producttoken)

    d = LicenseServiceClient(creds).verify_subscription_by_tokens(usertoken, producttoken)

    def verified(active):
        update_status(
            public = 'DevPay License subscription active? %r\n' % (active,))
        return active
    d.addCallback(verified)
    return d
