from txaws.ec2.client import EC2Client
from txaws.credentials import AWSCredentials
from txaws.service import AWSServiceEndpoint
from signup import EC2_ENDPOINT
import simplejson

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

    raise Exception("signup not found")


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

def delete_ec2_instance(ec2_accesskeyid, ec2_secretkey, instanceid):
    endpoint_uri = EC2_ENDPOINT
    ec2creds = AWSCredentials(ec2_accesskeyid, ec2_secretkey)
    endpoint = AWSServiceEndpoint(uri=endpoint_uri)
    client = EC2Client(creds=ec2creds, endpoint=endpoint)
    try:
        client.terminate_instances(instanceid)
    except Exception, e:
        if DEBUG: print str(e)

def terminate_customer_server(email, customerid, secretsdirfp):
    signupfp = find_customer_signup(email, customerid, secretsdirfp)
    instanceid = find_instance_id_for_signup(signupfp)

    print "terminating %r" % (instanceid,)
