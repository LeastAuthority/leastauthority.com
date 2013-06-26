import subprocess, os, urllib

from twisted.internet import reactor, task, defer
from twisted.python.filepath import FilePath

from lae_automation.server import install_infrastructure_server
from lae_automation.aws.license_service_client import LicenseServiceClient
from lae_automation.aws.devpay_s3client import DevPayS3Client
from lae_automation.aws.queryapi import xml_parse, xml_find, wait_for_EC2_sshfp, TimeoutError, \
     wait_for_EC2_addresses

from txaws.ec2.client import EC2Client
from txaws.ec2.model import Instance
from txaws.service import AWSServiceEndpoint
from txaws.credentials import AWSCredentials


class PublicKeyMismatch(Exception):
    pass


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


def deploy_EC2_instance(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id, instance_size,
                        bucket_name, keypair_name, instance_name, stdout, stderr, clock=None):
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


def deploy_infrastructure_EC2(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id, instance_size,
                              bucket_name, keypair_name, instance_name, admin_privkey_path, 
                              git_repository, commit_tag, stdout, stderr, clock=None):
    myclock = clock or reactor
    d = deploy_EC2_instance(ec2accesskeyid, ec2secretkey, endpoint_uri, ami_image_id,
                        instance_size, bucket_name, keypair_name, instance_name,
                        stdout, stderr)

    def _deployed(instance):
        ADDRESS_DELAY_TIME = 75
        POLL_TIME = 30
        ADDRESS_WAIT_TIME = 5 * 60
        d1 = task.deferLater(myclock, ADDRESS_DELAY_TIME, wait_for_EC2_addresses,
                             ec2accesskeyid, ec2secretkey, endpoint_uri, stdout, stderr, POLL_TIME,
                             ADDRESS_WAIT_TIME, instance.instance_id)
        print >>stdout, "instance is %s" % instance
        def  _got_addresses(addresses):
            CONSOLE_ACCESS_DELAY_TIME = 155
            address = addresses[0][0]
            print >>stdout, "Waiting %d seconds for the server to be ready..." % (CONSOLE_ACCESS_DELAY_TIME,)
            d2 = task.deferLater(reactor, CONSOLE_ACCESS_DELAY_TIME, verify_and_store_serverssh_pubkey, 
                                 ec2accesskeyid, ec2secretkey, endpoint_uri, address, 5, 20, 
                                 stdout, stderr, instance.instance_id)

            def _pubkey_verified(ignore):
                install_infrastructure_server(address, admin_privkey_path, git_repository, 
                                              commit_tag, stdout, stderr)
                

            d2.addCallback(_pubkey_verified)

            return d2

        d1.addCallback(_got_addresses)

    d.addCallback(_deployed)
    return d
    

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


def verify_and_store_serverssh_pubkey(ec2accesskeyid, ec2secretkey, endpoint_uri, pub_ipaddress,
                                      polling_interval, total_wait_time, stdout, stderr,
                                      instance_id):
    """
    Theory of Operation:  What the function intends.
    When a new ssh connection is established it is possible for a recipient-other-than-the-intended
    (a M.an I.n T.he M.iddle) to offer a counterfeit encryption key to the Connector.  This would
    enable the MITM to read the contents of the ssh traffic the Connector intended to transmit
    confidentially.

    If the Connector has an alternate connection (side-channel) to the intended recipient the
    Connector can check that the encryption key presented to it, is the same in both channels.
    This means that an attacker would have to control both channels to undetectably deceive the
    Connector.

    Theory of Operation:  How the function accomplishes its intention.
    When Least Authority (the "Connector") generates a new EC2 instance using the AWS REST interface
    it communicates over an https channel.  This means that Least Authority _COULD_ be assured by a
    'Certificate Authority' that 'AWS' is the initial recipient of Least Authority's confidential
    requests.

    Subsequent to initial generation, Least Authority uses ssh to communicate with its EC2s.

    When Least Authority establishes an initial ssh connection with an EC2 instance, it has
    generated a second, unauthenticated channel.  Least Authority uses the first, over-https,
    channel to obtain a copy of the ssh public key fingerprint that AWS asserts will be used
    by the new EC2.

    (NOTE: Although 'Least Authority' uses an https endpoint, i.e. protocol, certificate checking is
    not yet implemented so we do _not_ yet have cryptographic assurance of the authenticity of the
    AWS REST API provider.)

    The User that owns the Least Authority process then obtains the over-https public key
    fingerprint, uses it to check the pubkey offered over the ssh channel, and store it in its
    .ssh/known_hosts file, along with the hashed identity of the host. (This is not a 'security'
    check since, the over https fingerprint is not authenticated.)

    @param ec2accesskeyid:  An identifier AWS uses to lookup our credentials.
    @param ec2secretkey:    The symmetric secret we use to sign our queries.
    @param endpoint_uri:    An endpoint is a URL that is the entry point for a web service.
    @param addressparser:   An object that is referred to by an EC2 client and used to parse
    data returned from queries.
    @param polling_interval: An int that sets how long to wait between repeated queries.
    @param total_wait_time:  The total time allotted to a query type (same for both fingerprint
    and IP retrievals).
    @param instance_id:      An AWS internal id of an EC2.
    """
    d = wait_for_EC2_sshfp(ec2accesskeyid, ec2secretkey, endpoint_uri, polling_interval,
                                   total_wait_time, stdout, stderr, instance_id)

    def _got_fingerprintfromconsole(fingerprint):
        fingerprint_from_AWS = fingerprint
        d1 = wait_for_and_store_pubkeyfp_from_keyscan(pub_ipaddress, polling_interval,
                                                      total_wait_time, stdout)

        def _verifyfp_and_write_pubkey( (fingerprint_from_keyscan, hashed_pubkey) ):
            if fingerprint_from_AWS != fingerprint_from_keyscan:
                raise PublicKeyMismatch()
            print >>stderr, "The ssh public key on the server has fingerprint: %s" % (fingerprint_from_keyscan,)
            known_hosts_filepath = FilePath(os.path.expanduser('~')).child('.ssh').child('known_hosts')
            known_hosts = known_hosts_filepath.getContent().rstrip('\n') + '\n'
            new_known_hosts = known_hosts + hashed_pubkey
            known_hosts_filepath.setContent(new_known_hosts)

        d1.addCallback(_verifyfp_and_write_pubkey)
        return d1

    d.addCallback(_got_fingerprintfromconsole)
    return d


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


def wait_for_and_store_pubkeyfp_from_keyscan(targetIP, polling_interval, total_wait_time, stdout):
    """
    Uses the keyscan utility to repeatedly scan an IP until the target either responds, or the
    scan times out.
    """
    def _wait(remaining_time):
        print >>stdout, "Checking server ssh public key..."
        d = defer.succeed(get_and_store_pubkeyfp_from_keyscan(targetIP, stdout))
        def _maybe_again(res):
            if res:
                return res
            if remaining_time <= 0:
                print >>stdout, "Timed out waiting for ssh-keyscan."
                raise TimeoutError()
            print >>stdout, "Waiting %d seconds before repeating ssh-keyscan..." % (polling_interval,)
            return task.deferLater(reactor, polling_interval, _wait, remaining_time - polling_interval)
        d.addCallback(_maybe_again)
        return d
    return _wait(total_wait_time)


PUBKEY_DIR = 'pubkeys'

def get_and_store_pubkeyfp_from_keyscan(targetIP, stdout):
    """
    Return the host's (ssh pubkey fingerprint, hashed public key, targetIP).
    Because of the interface to ssh-keygen, this function creates files locally
    under PUBKEY_DIR.
    """
    pubkey_dir = FilePath(PUBKEY_DIR)
    try:
        pubkey_dir.makedirs()
    except OSError, e:
        if e.errno != os.errno.EEXIST:
            raise

    pubkey_filename = 'sshpubkey_'+targetIP
    pubkey_filepath = pubkey_dir.child(pubkey_filename)
    pubkey_relpath = os.path.join(PUBKEY_DIR, pubkey_filename)
    output = pubkey_filepath.open('w')
    keyscan_call = ('ssh-keyscan', '-H', targetIP)
    sp = subprocess.Popen(keyscan_call, stdout=output, stderr=subprocess.PIPE)
    if sp.wait() != 0:
        raise subprocess.CalledProcessError
    if pubkey_filepath.getContent() == '':
        return None

    keygen_call = ('ssh-keygen', '-l', '-f', pubkey_relpath)
    sp2 = subprocess.Popen(keygen_call, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                           stdin=sp.stdout)
    if sp2.wait() != 0:
        raise subprocess.CalledProcessError
    spoutput = sp2.stdout.read()
    fingerprint = spoutput.split()[1]

    sshkeygen_hash_call = ('ssh-keygen', '-H', '-f', pubkey_relpath)
    sp = subprocess.Popen(sshkeygen_hash_call)
    if sp.wait() != 0:
        raise subprocess.CalledProcessError

    hashed_pubkey = pubkey_filepath.getContent().rstrip('\n') + '\n'

    return (fingerprint, hashed_pubkey)
