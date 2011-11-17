import logging, urllib

from lae_site.aws.license_service_client import LicenseServiceClient
from lae_site.aws.devpay_s3client import DevPayS3Client

from txaws.ec2.client import EC2Client
from txaws.ec2.model import Instance
from txaws.service import AWSServiceEndpoint


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


def deploy_EC2_instance(creds, endpoint_uri, ami_image_id, instance_size, customer_email_id, keypair_name, status_callback):
    """
    @param creds: a txaws.service.AWSCredentials object
    @param ami_image_id: string identifying the AMI
    @param customer_email_id: identifier that is unique to a specific customer account. See e.g. howto setup instance.
    @param keypair_name: the name of the SSH keypair to be used
    """
    #logging.basicConfig(stream = sys.stdout, level = logging.DEBUG)
    log = logging.getLogger('deploy_EC2_instance')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback(public)

    mininstancecount = 1
    maxinstancecount = 1
    secgroups = ['CustomerDefault']
    update_status(public = 'Deploying EC2 instance...', EC2name = customer_email_id)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=creds, endpoint=endpoint)
    d = client.run_instances(ami_image_id,
                             mininstancecount,
                             maxinstancecount,
                             secgroups,
                             keypair_name,
                             instance_type=instance_size)

    def deployed(instance, *args, **kw):
        info = dump_instance_information(instance)
        update_status(
            public = 'EC2_deployed.',
            info = info)
        #self.instance_id = instance.instance_id
        #addressfp = FilePath(customer_email_id+'_EC2')
    d.addCallback(deployed)

    return d


def dump_instance_information(instance):
    if not isinstance(instance, Instance):
        return "<not an instance: %r>" % (instance,)
    desc = {}
    for attr in ('instance_id', 'instance_state', 'instance_type', 'image_id', 'private_dns_name',
                 'dns_name', 'key_name', 'ami_launch_index', 'launch_time', 'placement',
                 'product_codes', 'kernel_id', 'ramdisk_id', 'reservation'):
        if hasattr(instance, attr):
            desc[attr] = getattr(instance, attr)
    return "<%r %r>" % (instance, desc)


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
