
import urllib, re

from lae_automation.aws.license_service_client import LicenseServiceClient
from lae_automation.aws.devpay_s3client import DevPayS3Client
from lae_automation.aws.queryapi import xml_parse, xml_find, ResponseParseError

from txaws.ec2.client import EC2Client, Parser as txaws_ec2_Parser
from txaws.ec2.model import Instance
from txaws.service import AWSServiceEndpoint



def activate_user_account_desktop(activationkey, producttoken, stdout, stderr):
    """
    @param activationkey:
            The activationkey sent from the user's browser upon completion
            of the DevPay signup process.
    @param stdout, stderr:
            Standard output (for user feedback) and error (for debugging) streams.

    @return:
            A Deferred which fires with an ActivateDesktopProductResponse upon
            successful user initialization.
    """

    print >>stdout, "Activating license..."
    print >>stderr, 'activationkey = %r' % (activationkey,)

    d = LicenseServiceClient().activate_desktop_product(activationkey, producttoken)

    def activated(adpr):
        print >>stdout, 'License activated.'
        print >>stderr, ('access_key_id = %r\n'
                         'secret_key = %r\n'
                         'usertoken = %r'
                         % (adpr.access_key_id, adpr.secret_key, adpr.usertoken))
        return adpr
    d.addCallback(activated)
    return d


def deploy_EC2_instance(creds, endpoint_uri, ami_image_id, instance_size, bucket_name, keypair_name,
                        stdout, stderr):
    """
    @param creds: a txaws.service.AWSCredentials object
    @param ami_image_id: string identifying the AMI
    @param bucket_name: identifier that is unique to a specific customer account
    @param keypair_name: the name of the SSH keypair to be used
    """

    print >>stdout, "Deploying EC2 instance..."

    mininstancecount = 1
    maxinstancecount = 1
    secgroups = ['CustomerDefault']
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=creds, endpoint=endpoint)

    # Don't log credentials that are non-customer-specific secrets.
    print >>stderr, ('endpoint_uri = %r\n'
                     'ami_image_id = %r\n'
                     'instance_size = %r\n'
                     'bucket_name = %r\n'
                     'keypair_name = %r'
                     % (endpoint_uri, ami_image_id, instance_size, bucket_name, keypair_name))

    d = client.run_instances(ami_image_id,
                             mininstancecount,
                             maxinstancecount,
                             secgroups,
                             keypair_name,
                             instance_type=instance_size)

    def started(instances, *args, **kw):
        print >>stdout, "EC2 instance started."
        for i in instances:
            dump_instance_information(i, stderr)

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


def dump_instance_information(instance, stderr):
    if not isinstance(instance, Instance):
        print >>stderr, "not an instance: %r" % (instance,)
    else:
        print >>stderr, repr(instance)
    for attr in ('instance_id', 'instance_state', 'instance_type', 'image_id', 'private_dns_name',
                 'dns_name', 'key_name', 'ami_launch_index', 'launch_time', 'placement',
                 'product_codes', 'kernel_id', 'ramdisk_id', 'reservation'):
        if hasattr(instance, attr):
            print >>stderr, '  %s = %r' % (attr, getattr(instance, attr))


def create_user_bucket(creds, usertoken, bucketname, stdout, stderr, producttoken=None, location=None):
    print >>stdout, "Creating S3 bucket..."
    print >>stderr, ('usertoken = %r\n'
                     'bucketname = %r\n'
                     'location = %r\n'
                     % (usertoken, bucketname, location))

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

    def bucket_created(res):
        print >>stdout, "S3 bucket created."
        print >>stderr, repr(res)

    d.addCallback(bucket_created)
    return d


def delete_user_bucket(creds, usertoken, bucketname, stdout, stderr, producttoken=None):
    print >>stdout, "Deleting S3 bucket..."
    print >>stderr, ('usertoken = %r\n'
                     'bucketname = %r\n'
                     % (usertoken, bucketname))

    client = DevPayS3Client(
        creds = creds,
        usertoken = usertoken,
        producttoken = producttoken)

    query = client.query_factory(
        action="DELETE", creds=client.creds, endpoint=client.endpoint,
        bucket=bucketname)
    d = query.submit()

    def bucket_deleted(res):
        print >>stdout, "S3 bucket deleted."
        print >>stderr, repr(res)

    d.addCallback(bucket_deleted)
    return d


def verify_user_account(creds, usertoken, producttoken, stdout, stderr):
    print >>stdout, "Verifying subscription..."
    print >>stderr, 'usertoken = %r' % (usertoken,)

    d = LicenseServiceClient(creds).verify_subscription_by_tokens(usertoken, producttoken)

    def verified(active):
        print >>stderr, 'DevPay License subscription active? %r' % (active,)
        return active
    d.addCallback(verified)
    return d
