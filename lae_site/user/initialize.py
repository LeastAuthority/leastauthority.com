
import logging, urllib, re

from lae_site.aws.license_service_client import LicenseServiceClient
from lae_site.aws.devpay_s3client import DevPayS3Client
from lae_site.aws.queryapi import xml_parse, xml_find, ResponseParseError

from txaws.ec2.client import EC2Client, Parser as txaws_ec2_Parser
from txaws.ec2.model import Instance
from txaws.service import AWSServiceEndpoint



def activate_user_account_desktop(activationkey, producttoken, status_callback):
    """
    @param activationkey:
            The activationkey sent from the user's browser upon completion
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
        status_callback("%r\n%r" % (public, private_details))

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


def deploy_EC2_instance(creds, endpoint_uri, ami_image_id, instance_size, bucket_name, keypair_name, status_callback):
    """
    @param creds: a txaws.service.AWSCredentials object
    @param ami_image_id: string identifying the AMI
    @param bucket_name: identifier that is unique to a specific customer account
    @param keypair_name: the name of the SSH keypair to be used
    """
    log = logging.getLogger('deploy_EC2_instance')

    def update_status(public, **private_details):
        log.info('Update Status: %r', public)
        log.info('Private Details: %r', private_details)
        status_callback("%r\n%r" % (public, private_details))

    mininstancecount = 1
    maxinstancecount = 1
    secgroups = ['CustomerDefault']
    update_status(public = 'Deploying EC2 instance...', bucket_name = bucket_name)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=creds, endpoint=endpoint)

    status_callback(repr((creds.access_key, creds.secret_key, endpoint_uri, ami_image_id, instance_size, bucket_name, keypair_name)))

    d = client.run_instances(ami_image_id,
                             mininstancecount,
                             maxinstancecount,
                             secgroups,
                             keypair_name,
                             instance_type=instance_size)

    def started(instances, *args, **kw):
        info = [dump_instance_information(i) for i in instances]
        update_status(
            public = 'EC2 instance started.',
            info = info)

        assert len(instances) == 1, len(instances)
        return instances[0]
    d.addCallback(started)
    return d


def get_EC2_addresses(creds, endpoint_uri, instance_id):
    """
    Reference: http://docs.amazonwebservices.com/AWSEC2/latest/APIReference/index.html?ApiReference-query-DescribeInstances.html
    """
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=creds, endpoint=endpoint, parser=AddressParser())
    return client.describe_instances(instance_id)


EC2_PUBLIC_DNS = re.compile(r'^ec2(-(0|([1-9][0-9]{0,2}))){4}\.')

class AddressParser(txaws_ec2_Parser):
    def describe_instances(self, xml_bytes):
        doc = xml_parse(xml_bytes)
        node = xml_find(doc, u'reservationSet')
        node = xml_find(node, u'item')
        node = xml_find(node, u'instancesSet')
        node = xml_find(node, u'item')
        try:
            publichost = xml_find(node, u'dnsName').text
            privatehost = xml_find(node, u'privateDnsName').text
        except ResponseParseError:
            return None

        if not publichost or not privatehost:
            return None

        publichost = publichost.strip()
        privatehost = privatehost.strip()
        m = EC2_PUBLIC_DNS.match(publichost)
        if m:
            # If the name matches EC2_PUBLIC_DNS, we prefer to extract the IP address
            # to eliminate the DNS point of failure.
            publichost = publichost[len('ec2-'):].split('.')[0].replace('-', '.')

        return (publichost, privatehost)


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
        status_callback("%r\n%r" % (public, private_details))

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
        status_callback("%r\n%r" % (public, private_details))

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
