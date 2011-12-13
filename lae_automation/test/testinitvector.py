mutoken = 'TEST'+'A'*394
makeyID = 'TEST'+'A'*16
msecakey = 'TEST'+'A'*36
mrequestid = 'TEST'+'A'*32
# Test vector responses to the different make http requests: activation, verification, describe instances
adprequestresponse = """<ActivateDesktopProductResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><ActivateDesktopProductResult><UserToken>{UserToken}%s==</UserToken><AWSAccessKeyId>%s</AWSAccessKeyId><SecretAccessKey>%s</SecretAccessKey></ActivateDesktopProductResult><ResponseMetadata><RequestId>%s</RequestId></ResponseMetadata></ActivateDesktopProductResponse>"""%(mutoken, makeyID, msecakey, mrequestid)
        
verifyrequestresponse = """<VerifyProductSubscriptionByTokensResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><VerifyProductSubscriptionByTokensResult><Subscribed>true</Subscribed></VerifyProductSubscriptionByTokensResult><ResponseMetadata><RequestId>bd9db94b-a1b0-4a5f-8d70-6cc4de427623</RequestId></ResponseMetadata></VerifyProductSubscriptionByTokensResponse>"""

describeEC2instresponse = """<?xml version="1.0" encoding="UTF-8"?><DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/"><requestId>TEST</requestId><reservationSet><item><reservationId>TEST</reservationId><ownerId>TEST</ownerId><groupSet><item><groupId>CustomerDefault</groupId></item></groupSet><instancesSet><item><instanceId>TEST</instanceId><imageId>TEST</imageId><instanceState><code>TEST</code><name>TEST</name></instanceState><privateDnsName>TESTinternal</privateDnsName><dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName><reason/><keyName>TEST</keyName><amiLaunchIndex>0</amiLaunchIndex><productCodes/><instanceType>t1.TEST</instanceType><launchTime>TEST</launchTime><placement><availabilityZone>TEST</availabilityZone></placement><kernelId>TEST</kernelId></item></instancesSet></item></reservationSet></DescribeInstancesResponse>"""
