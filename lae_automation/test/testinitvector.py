MUTOKEN = 'TESTUSERTOKEN'+'A'*385
MAKEYID = 'TEST'+'A'*16
MSECAKEY = 'TEST'+'A'*36
MREQUESTID = 'TEST'+'A'*32
# Test vector requests and responses to the different make http requests: activation, verification, describe instances

#ACTIVATION

adprequestresponse = """<ActivateDesktopProductResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><ActivateDesktopProductResult><UserToken>{UserToken}%s==</UserToken><AWSAccessKeyId>%s</AWSAccessKeyId><SecretAccessKey>%s</SecretAccessKey></ActivateDesktopProductResult><ResponseMetadata><RequestId>%s</RequestId></ResponseMetadata></ActivateDesktopProductResponse>"""%(MUTOKEN, MAKEYID, MSECAKEY, MREQUESTID)

PRODUCTTOKEN = 'TESTPRODUCTTOKEN'+'A'*295
adphttprequestheader = """https://ls.amazonaws.com/?Action=ActivateDesktopProduct&ActivationKey=MOCKACTIVATONKEY&ProductToken=%7BProductToken%7DTESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D&Version=2008-04-28"""


verifyhttprequestheader = """https://ls.amazonaws.com/?Action=VerifyProductSubscriptionByTokens&AWSAccessKeyId=TESTAAAAAAAAAAAAAAAA&Expires=1970-01-01T00%3A15%3A00Z&ProductToken=%7BProductToken%7DTESTPRODUCTTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D&SignatureVersion=1&UserToken=%7BUserToken%7DTESTUSERTOKENAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA%3D%3D&Version=2008-04-28&Signature=EoWZTlMO9qQA6Pbq5Ze4eHAlKZc%3D"""
        
verifyrequestresponse = """<VerifyProductSubscriptionByTokensResponse xmlns="http://ls.amazonaws.com/doc/2008-04-28/"><VerifyProductSubscriptionByTokensResult><Subscribed>true</Subscribed></VerifyProductSubscriptionByTokensResult><ResponseMetadata><RequestId>bd9db94b-a1b0-4a5f-8d70-6cc4de427623</RequestId></ResponseMetadata></VerifyProductSubscriptionByTokensResponse>"""


#DESCRIBE

describeEC2instresponse = """<?xml version="1.0" encoding="UTF-8"?><DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/"><requestId>TEST</requestId><reservationSet><item><reservationId>TEST</reservationId><ownerId>TEST</ownerId><groupSet><item><groupId>CustomerDefault</groupId></item></groupSet><instancesSet><item><instanceId>TEST</instanceId><imageId>TEST</imageId><instanceState><code>TEST</code><name>TEST</name></instanceState><privateDnsName>TESTinternal</privateDnsName><dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName><reason/><keyName>TEST</keyName><amiLaunchIndex>0</amiLaunchIndex><productCodes/><instanceType>t1.TEST</instanceType><launchTime>TEST</launchTime><placement><availabilityZone>TEST</availabilityZone></placement><kernelId>TEST</kernelId></item></instancesSet></item></reservationSet></DescribeInstancesResponse>"""
