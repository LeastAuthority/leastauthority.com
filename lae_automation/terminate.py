import simplejson

from twisted.python.filepath import FilePath
from txaws.ec2.client import EC2Client
from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint

from lae_automation.signup import EC2_ENDPOINT
from lae_automation.config import Config

DEBUG = True


def find_customer_signup(email, customerid, secretsdirfp):
    for childfp in secretsdirfp.children():
        stripefp = childfp.child("stripe")
        try:
            stripejson = simplejson.loads(stripefp.getContent())
            thisemail = stripejson[0]
            thiscustomerid = stripejson[2]
            if DEBUG: print "customer %r with id %r" % (thisemail, thiscustomerid)
            if (thisemail, thiscustomerid) == (email, customerid):
                if DEBUG: print "found %r" % (childfp.path,)
                return childfp
        except Exception, e:
            if DEBUG: print str(e)

    raise KeyError("signup not found")


def find_instance_id_for_signup(signupfp):
    logfp = signupfp.child('signup_logs')
    loglines = logfp.getContent().splitlines()
    for (num, line) in enumerate(loglines):
        if line.startswith('<txaws.ec2.model.Instance object at ') and num+1 < len(loglines):
            instanceline = loglines[num+1]
            (before, _a, after) = instanceline.partition("'")
            if before.endswith('instance_id = '):
                (instanceid, _b, _c) = after.partition("'")
                if DEBUG: print "instance id %r" % (instanceid,)
                return instanceid

    raise Exception("instanceid not found")

def delete_ec2_instance(config, instanceid):
    endpoint_uri = EC2_ENDPOINT
    ec2creds = load_ec2_credentials(config)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=ec2creds, endpoint=endpoint)
    print "here"
    return client.terminate_instances(instanceid)

def load_ec2_credentials(config):
    accesskeyid = config.other["ssec2_access_key_id"]
    secretpath = config.other["ssec2_secret_path"]
    secretkey = FilePath(secretpath).getContent().strip()

    creds = AWSCredentials(accesskeyid, secretkey)
    return creds

def terminate_customer_server(email, customerid, secretsdirfp, configpath=None):
    config = Config(configpath)
    signupfp = find_customer_signup(email, customerid, secretsdirfp)
    instanceid = find_instance_id_for_signup(signupfp)

    print "terminating %r" % (instanceid,)
    return delete_ec2_instance(config, instanceid)
