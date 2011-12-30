from lae_automation.config import Config
from txaws.service import AWSCredentials, AWSServiceEndpoint
from lae_automation.aws.queryapi import AddressParser, get_EC2_addresses
from twisted.python.filepath import FilePath
from twisted.internet import reactor
from server import upgrade_server
import sys, os

EC2adminkeyfilen = '../EC2adminkeys2.pem'
monsshpubkey = FilePath('../EC2monitorssh_key.pub').getContent().strip()
endpoint_uri = 'https://ec2.us-east-1.amazonaws.com/'
configpath='../lae_automation_config.json'
config = Config(configpath)
# For the admin of AWS, ec2secret XXX permits login to AWS?
ec2secretpath='../ec2secret'
ec2accesskeyid = str(config.other['ec2_access_key_id'])
ec2secretkey = FilePath(ec2secretpath).getContent().strip()

ec2creds = AWSCredentials(ec2accesskeyid, ec2secretkey)
endpoint = AWSServiceEndpoint(endpoint_uri)
parser = AddressParser()


d = get_EC2_addresses(ec2accesskeyid, ec2secretkey, endpoint_uri)

def upgrade_servers(public_host_list):
    for public_host in public_host_list[:1]:
        printer(public_host, EC2adminkeyfilen, monsshpubkey, sys.stdout, sys.stderr)

d.addCallback(upgrade_servers)


def printer(*args):
    for index, arg in enumerate(args):
        print "arg %s is %s." % (index, arg)

def errprinter(x):
    print x
    import sys
    reactor.stop()
    sys.exit(89)

d.addCallback(printer)
d.addErrback(errprinter)
d.addCallbacks(lambda ign: os._exit(0), lambda ign: os._exit(1))
reactor.run()
