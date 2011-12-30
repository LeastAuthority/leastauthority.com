import urllib, re
from twisted.internet import reactor, task

from lae_automation.aws.license_service_client import LicenseServiceClient
from lae_automation.aws.devpay_s3client import DevPayS3Client
from lae_automation.aws.queryapi import xml_parse, xml_find, ResponseParseError, AddressParser

from txaws.ec2.client import EC2Client
from txaws.ec2.model import Instance
from txaws.service import AWSServiceEndpoint
from txaws.credentials import AWSCredentials



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


# delay between starting an instance and setting its tags
SET_TAGS_DELAY_TIME = 5

def deploy_EC2_instance(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id, instance_size, bucket_name,
                        keypair_name, instance_name, stdout, stderr, clock=None):
    """
    @param creds: a txaws.service.AWSCredentials object
    @param ami_image_id: string identifying the AMI
    @param bucket_name: identifier that is unique to a specific customer account
    @param keypair_name: the name of the SSH keypair to be used
    """
    myclock = clock or reactor

    print >>stdout, "Deploying EC2 instance..."

    mininstancecount = 1
    maxinstancecount = 1
    secgroups = ['CustomerDefault']
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)
    client = EC2Client(creds=ec2creds, endpoint=endpoint)

    # Don't log credentials that are non-customer-specific secrets.
    print >>stderr, ('endpoint_uri = %r\n'
                     'ami_image_id = %r\n'
                     'instance_size = %r\n'
                     'bucket_name = %r\n'
                     'keypair_name = %r\n'
                     'instance_name = %r\n'
                     % (endpoint_uri, ami_image_id, instance_size, bucket_name, keypair_name, instance_name))

    d = client.run_instances(ami_image_id,
                             mininstancecount,
                             maxinstancecount,
                             secgroups,
                             keypair_name,
                             instance_type=instance_size)

    def started(instances):
        print >>stdout, "EC2 instance started."
        for i in instances:
            dump_instance_information(i, stderr)

        assert len(instances) == 1, len(instances)
        return instances[0]
    d.addCallback(started)

    def set_tags(instance):
        instance_id = instance.instance_id
        query = client.query_factory(
            action='CreateTags', creds=client.creds, endpoint=client.endpoint,
            other_params={'Version': '2011-11-01',
                          'ResourceId.1': instance_id,
                          'Tag.1.Key': 'Name', 'Tag.1.Value': instance_name,
                          'Tag.2.Key': 'Bucket', 'Tag.2.Value': bucket_name})
        d2 = query.submit()

        def parse_response(xml_bytes):
            doc = xml_parse(xml_bytes)
            ret = xml_find(doc, u'return').text.strip()
            if ret == 'true':
                return instance
            else:
                raise AssertionError("could not set tags for %r" % (instance_id,))
        d2.addCallback(parse_response)
        return d2
    d.addCallback(lambda instance: task.deferLater(myclock, SET_TAGS_DELAY_TIME, set_tags, instance))
    return d


def get_EC2_addresses(creds, endpoint_uri, instance_id):
    """
    Reference: http://docs.amazonwebservices.com/AWSEC2/latest/APIReference/index.html?ApiReference-query-DescribeInstances.html
    """
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=creds, endpoint=endpoint, parser=AddressParser())
    return client.describe_instances(instance_id)


EC2_PUBLIC_DNS = re.compile(r'^ec2(-(0|([1-9][0-9]{0,2}))){4}\.')


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


def create_user_bucket(useraccesskeyid, usersecretkey, usertoken, bucketname, stdout, stderr,
                       producttoken=None, location=None):
    if location is None:
        print >>stdout, "Creating S3 bucket in 'US East' region..."
    else:
        # TODO: print user-friendly region name
        print >>stdout, "Creating S3 bucket..."

    print >>stderr, ('usertoken = %r\n'
                     'bucketname = %r\n'
                     'location = %r\n'
                     % (usertoken, bucketname, location))

    usercreds = AWSCredentials(useraccesskeyid, usersecretkey)
    client = DevPayS3Client(creds=usercreds, usertoken=usertoken, producttoken=producttoken)

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


def delete_user_bucket(useraccesskeyid, usersecretkey, usertoken, bucketname, stdout, stderr,
                       producttoken=None):
    print >>stdout, "Deleting S3 bucket..."
    print >>stderr, ('usertoken = %r\n'
                     'bucketname = %r\n'
                     % (usertoken, bucketname))

    usercreds = AWSCredentials(useraccesskeyid, usersecretkey)
    client = DevPayS3Client(creds=usercreds, usertoken=usertoken, producttoken=producttoken)

    query = client.query_factory(
        action="DELETE", creds=client.creds, endpoint=client.endpoint,
        bucket=bucketname)
    d = query.submit()

    def bucket_deleted(res):
        print >>stdout, "S3 bucket deleted."
        print >>stderr, repr(res)

    d.addCallback(bucket_deleted)
    return d


def verify_user_account(useraccesskeyid, usersecretkey, usertoken, producttoken, stdout, stderr):
    print >>stdout, "Verifying subscription..."
    print >>stderr, 'usertoken = %r' % (usertoken,)

    usercreds = AWSCredentials(useraccesskeyid, usersecretkey)
    d = LicenseServiceClient(usercreds).verify_subscription_by_tokens(usertoken, producttoken)

    def verified(active):
        print >>stderr, 'DevPay License subscription active? %r' % (active,)
        return active
    d.addCallback(verified)
    return d
