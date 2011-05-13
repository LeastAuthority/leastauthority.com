from twisted.trial.unittest import TestCase

import mock

from lae_site.aws.devpay_s3client import DevpayS3Client


class DevpayS3ClientTests (TestCase):

    @mock.patch('lae_site.aws.devpay_s3client.Query')
    def test_create_bucket_has_devpay_header(self, mockquery):

        mockcreds = mock.Mock(name='MockCreds')
        devpayusertoken = mock.sentinel.DEVPAYUSERTOKEN

        s3c = DevpayS3Client(mockcreds, devpayusertoken)

        bucketname = 'global_shared_namespaces_are_silly'
        s3c.create_bucket(bucketname)

        (args, kw) = mockquery.call_args
        self.failUnless('amz_headers' in kw)

        amz_headers = kw['amz_headers']
        self.failUnless('security-token' in amz_headers)

        sectoken = amz_headers['security-token']
        self.failUnless(sectoken is devpayusertoken)
        
        




        

