
from base64 import b32encode

import attr

from twisted.trial.unittest import TestCase
from twisted.internet import defer
from twisted.python.filepath import FilePath
from twisted.python.failure import Failure

from lae_util.fileutil import make_dirs
from lae_util.streams import LoggingStream
from lae_automation import signup, initialize

from lae_automation.subscription_manager import memory_client

# Vector data for request responses: activate desktop-, verify-, and describeEC2- responses.
ACCESSKEYID = 'TEST'+'A'*16
SECRETACCESSKEY = 'TEST'+'A'*36
REQUESTID = 'TEST'+'A'*32

# Test vector request and response to the make http request: describe instances

# DescribeInstances
describeEC2instresponse = """<?xml version="1.0" encoding="UTF-8"?>
<DescribeInstancesResponse xmlns="http://ec2.amazonaws.com/doc/2008-12-01/">
  <requestId>TEST</requestId>
  <reservationSet>
    <item>
      <reservationId>TEST</reservationId>
      <ownerId>TEST</ownerId>
      <groupSet><item><groupId>CustomerDefault</groupId></item></groupSet>
      <instancesSet>
        <item>
          <instanceId>TEST</instanceId>
          <imageId>TEST</imageId>
          <instanceState><code>TEST</code><name>TEST</name></instanceState>
          <privateDnsName>TESTinternal</privateDnsName>
          <dnsName>ec2-50-17-175-164.compute-1.amazonaws.com</dnsName>
          <reason/>
          <keyName>TEST</keyName>
          <amiLaunchIndex>0</amiLaunchIndex>
          <productCodes/>
          <instanceType>t1.TEST</instanceType>
          <launchTime>TEST</launchTime>
          <placement><availabilityZone>TEST</availabilityZone></placement>
          <kernelId>TEST</kernelId>
        </item>
      </instancesSet>
    </item>
  </reservationSet>
</DescribeInstancesResponse>"""

# CreateTags
createtagsresponse = """<CreateTagsResponse xmlns="http://ec2.amazonaws.com/doc/2011-11-01/">
  <requestId>7a62c49f-347e-4fc4-9331-6e8eEXAMPLE</requestId>
  <return>true</return>
</CreateTagsResponse>"""

# Get Console
getconsoleoutputresponse = """<GetConsoleOutputResponse xmlns="http://ec2.amazonaws.com/doc/2013-02-01/">
  <requestId>59dbff89-35bd-4eac-99ed-be587EXAMPLE</requestId>
  <instanceId>i-MOCKTEST</instanceId>
  <timestamp>2010-10-14T01:12:41.000Z</timestamp>
  <output>MjAxMy0wNC0xMSAyMDozMjoyMiwxMTYgLSBfX2luaXRfXy5weVtXQVJOSU5HXTogVW5oYW5kbGVk\nIG5vbi1tdWx0aXBhcnQgdXNlcmRhdGEgJycNCkdlbmVyYXRpbmcgcHVibGljL3ByaXZhdGUgcnNh\nIGtleSBwYWlyLg0KWW91ciBpZGVudGlmaWNhdGlvbiBoYXMgYmVlbiBzYXZlZCBpbiAvZXRjL3Nz\naC9zc2hfaG9zdF9yc2Ffa2V5Lg0KWW91ciBwdWJsaWMga2V5IGhhcyBiZWVuIHNhdmVkIGluIC9l\ndGMvc3NoL3NzaF9ob3N0X3JzYV9rZXkucHViLg0KVGhlIGtleSBmaW5nZXJwcmludCBpczoNCmI4\nOjgzOmNmOjFkOjk3OjRiOjQ0OjhmOmE3OjA1OjI5OjRlOmY2OjFlOmFmOmRkIHJvb3RAaXAtMTAt\nMTk0LTI5LTM5DQpUaGUga2V5J3MgcmFuZG9tYXJ0IGltYWdlIGlzOg0KKy0tWyBSU0EgMjA0OF0t\nLS0tKw0KfCAgICAgICAgICAgICAgICAgfA0KfCAgICAgICAgICAgLiAgICAgfA0KfCAgICAgICAg\nKyArICAgICAgfA0KfCAgICAgICA9ICsgKyAgICAgfA0KfCAgICAgIC4gUyA9ICsgICAgfA0KfCAg\nICAgLiAuIG8gQiAgICAgfA0KfCAgICAuIG8gLiAqIC4gICAgfA0KfCAgICAgbyBvICsgKyAuICAg\nfA0KfCAgICAgIG8gLiBvIC4gRSAgfA0KKy0tLS0tLS0tLS0tLS0tLS0tKw0KR2VuZXJhdGluZyBw\ndWJsaWMvcHJpdmF0ZSBkc2Ega2V5IHBhaXIuDQpZb3VyIGlkZW50aWZpY2F0aW9uIGhhcyBiZWVu\nIHNhdmVkIGluIC9ldGMvc3NoL3NzaF9ob3N0X2RzYV9rZXkuDQpZb3VyIHB1YmxpYyBrZXkgaGFz\nIGJlZW4gc2F2ZWQgaW4gL2V0Yy9zc2gvc3NoX2hvc3RfZHNhX2tleS5wdWIuDQpUaGUga2V5IGZp\nbmdlcnByaW50IGlzOg0KMjU6ZmQ6Nzk6MjE6MmM6NjI6ZDI6MGQ6NzI6MGE6NGM6NTg6MGI6NmE6\nNWM6MjAgcm9vdEBpcC0xMC0xOTQtMjktMzkNClRoZSBrZXkncyByYW5kb21hcnQgaW1hZ2UgaXM6\nDQorLS1bIERTQSAxMDI0XS0tLS0rDQp8RS5vKisgLiBvICAgICAgICB8DQp8by5vLi5vID0gKyAu\nICAgICB8DQp8Lm8gIC4gbyA9ID0gbyAuICB8DQp8LiAgICAgIG8gKyBvIG8gLiB8DQp8ICAgICAg\nICBTICAgbyAuICB8DQp8ICAgICAgICAgICAgIC4gICB8DQp8ICAgICAgICAgICAgICAgICB8DQp8\nICAgICAgICAgICAgICAgICB8DQp8ICAgICAgICAgICAgICAgICB8DQorLS0tLS0tLS0tLS0tLS0t\nLS0rDQpHZW5lcmF0aW5nIHB1YmxpYy9wcml2YXRlIGVjZHNhIGtleSBwYWlyLg0KWW91ciBpZGVu\ndGlmaWNhdGlvbiBoYXMgYmVlbiBzYXZlZCBpbiAvZXRjL3NzaC9zc2hfaG9zdF9lY2RzYV9rZXku\nDQpZb3VyIHB1YmxpYyBrZXkgaGFzIGJlZW4gc2F2ZWQgaW4gL2V0Yy9zc2gvc3NoX2hvc3RfZWNk\nc2Ffa2V5LnB1Yi4NClRoZSBrZXkgZmluZ2VycHJpbnQgaXM6DQo0MjpjOTowZTpiNTozMzo3NDo1\nZDowMDpkODo1ODowZTo1MDozMjpiNTpiNDoyNiByb290QGlwLTEwLTE5NC0yOS0zOQ0KVGhlIGtl\neSdzIHJhbmRvbWFydCBpbWFnZSBpczoNCistLVtFQ0RTQSAgMjU2XS0tLSsNCnwgICAgKytCPStv\nLm8uICAgIHwNCnwgICAgIEJvTy4gLiAgICAgIHwNCnwgICAgRSBAIC4gICAgICAgIHwNCnwgICAg\nICogbyAgICAgICAgIHwNCnwgICAgICBvIFMgICAgICAgIHwNCnwgICAgICAgLiAgICAgICAgIHwN\nCnwgICAgICAgICAgICAgICAgIHwNCnwgICAgICAgICAgICAgICAgIHwNCnwgICAgICAgICAgICAg\nICAgIHwNCistLS0tLS0tLS0tLS0tLS0tLSsNClNraXBwaW5nIHByb2ZpbGUgaW4gL2V0Yy9hcHBh\ncm1vci5kL2Rpc2FibGU6IHVzci5zYmluLnJzeXNsb2dkDQogKiBTdGFydGluZyBBcHBBcm1vciBw\ncm9maWxlcyAgICAgICAbWzgwRyANG1s3NEdbIE9LIF0NCmxhbmRzY2FwZS1jbGllbnQgaXMgbm90\nIGNvbmZpZ3VyZWQsIHBsZWFzZSBydW4gbGFuZHNjYXBlLWNvbmZpZy4NCkdlbmVyYXRpbmcgbG9j\nYWxlcy4uLgogIGVuX1VTLlVURi04Li4uIGRvbmUKR2VuZXJhdGlvbiBjb21wbGV0ZS4KZWMyOiAK\nZWMyOiAjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMj\nIyMjIyMjIyMjCmVjMjogLS0tLS1CRUdJTiBTU0ggSE9TVCBLRVkgRklOR0VSUFJJTlRTLS0tLS0K\nZWMyOiAxMDI0IDI1OmZkOjc5OjIxOjJjOjYyOmQyOjBkOjcyOjBhOjRjOjU4OjBiOjZhOjVjOjIw\nICByb290QGlwLTEwLTE5NC0yOS0zOSAoRFNBKQplYzI6IDI1NiA0MjpjOTowZTpiNTozMzo3NDo1\nZDowMDpkODo1ODowZTo1MDozMjpiNTpiNDoyNiAgcm9vdEBpcC0xMC0xOTQtMjktMzkgKEVDRFNB\nKQplYzI6IDIwNDggYjg6ODM6Y2Y6MWQ6OTc6NGI6NDQ6OGY6YTc6MDU6Mjk6NGU6ZjY6MWU6YWY6\nZGQgIHJvb3RAaXAtMTAtMTk0LTI5LTM5IChSU0EpCmVjMjogLS0tLS1FTkQgU1NIIEhPU1QgS0VZ\nIEZJTkdFUlBSSU5UUy0tLS0tCmVjMjogIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMj\nIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIwotLS0tLUJFR0lOIFNTSCBIT1NUIEtFWSBLRVlT\nLS0tLS0KZWNkc2Etc2hhMi1uaXN0cDI1NiBBQUFBRTJWalpITmhMWE5vWVRJdGJtbHpkSEF5TlRZ\nQUFBQUlibWx6ZEhBeU5UWUFBQUJCQkd6d1l6WjF1eXVDSHJNR3ZHZHQyazFKakRhUTl1R2RGQXM2\nQ3k3dlkyTW95c3IxbmZaVE1KME5BV2I4MlkxZ3I0amZqbUIwY1BtQks4VGcxd2Urb1BvPSByb290\nQGlwLTEwLTE5NC0yOS0zOQpzc2gtcnNhIEFBQUFCM056YUMxeWMyRUFBQUFEQVFBQkFBQUJBUUNn\nb0M4OXZlNjFVaS9VaTVwbklTdmlkWVcvdVVyVFN6MEYwajBjenV6RHlTUGwvUnF4S3VadllRQWp1\nZzZPRW4wT2htbmg0Mmo2RGs0THIwOUg5R0xKVHVEdjJrVE5oUUY5ODBSWFVQcTNOVHlvengxc3A4\nMjRYM3pHK2VQaWlZejJPUUkvQ2YzcjJjQVVxZ2dKakE4d1BhV1NYeXV5cC9MczI4NjFYdnNuUzVp\nSVNueWxwRXVOL09YTkZSbXVKUnpzK2hjazV1ck1Weno1QWRibjl6U0svTkpSUFlTM3FTZnZDTjZw\nWEJ5TGNFSlVFTkNLSUE4MFZSSEtudi9pc0tzY0Fadm1ESkNvclJXWFJ2eEI2eExCMlZuVGJxQlVU\nUWErMHpyemZRU0pEOEMyMlliUGxnd3NGdG4wNzl3T2trYVF0ZC9BaGhQWGZUbEFVbGpBZkMvQiBy\nb290QGlwLTEwLTE5NC0yOS0zOQotLS0tLUVORCBTU0ggSE9TVCBLRVkgS0VZUy0tLS0tCmNsb3Vk\nLWluaXQgYm9vdCBmaW5pc2hlZCBhdCBUaHUsIDExIEFwciAyMDEzIDIwOjMyOjMyICswMDAwLiBV\ncCAyOC4yNCBzZWNvbmRzCg==\n</output>
</GetConsoleOutputResponse>"""

MOCKSERVERSSHFP = 'b8:83:cf:1d:97:4b:44:8f:a7:05:29:4e:f6:1e:af:dd'
MOCKHASHEDPUBKEY = """|1|lrzohCU8y8Obch3wa7+gnvEJuI0=|I1GQU+vw3MgMnyvY+SxnhCyArHg= ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCgoC89ve61Ui/Ui5pnISvidYW/uUrTSz0F0j0czuzDySPl/RqxKuZvYQAjug6OEn0Ohmnh42j6Dk4Lr09H9GLJTuDv2kTNhQF980RXUPq3NTyozx1sp824X3zG+ePiiYz2OQI/Cf3r2cAUqggJjA8wPaWSXyuyp/Ls2861XvsnS5iISnylpEuN/OXNFRmuJRzs+hck5urMVzz5Adbn9zSK/NJRPYS3qSfvCN6pXByLcEJUENCKIA80VRHKnv/isKscAZvmDJCorRWXRvxB6xLB2VnTbqBUTQa+0zrzfQSJD8C22YbPlgwsFtn079wOkkaQtd/AhhPXfTlAUljAfC/B"""

# Vector data for the config file data:
from lae_automation.test.test_vectors import MOCKJSONCONFIGFILE
CONFIGFILEJSON = MOCKJSONCONFIGFILE

ZEROPRODUCT = {
  "products": [],
  "ec2_access_key_id":    "TESTEC2EC2EC2EC2EC2E",
  "ec2_secret_path":      "mock_ec2_secret",
  "s3_access_key_id":     "TESTS3S3S3S3S3S3S3S3",
  "s3_secret_path":       "mock_s3_secret",
  "admin_keypair_name":   "ADMINKEYS",
  "admin_privkey_path":   "ADMINKEYS.pem",
  "monitor_pubkey_path":  "MONITORKEYS.pub",
  "monitor_privkey_path": "MONITORKEYS.pem"
}

MOCKEC2SECRETCONTENTS = 'EC2'*13+'E'
MOCKS3SECRETCONTENTS = 'S3'*20
MONITORPUBKEY = 'MONITOR PUBLIC KEY'


class TestSignupModule(TestCase):
    def setUp(self):
        self.mockconfigdir = FilePath('./test_signup').child('TestSignupModule')
        make_dirs(self.mockconfigdir.path)
        self.SIGNUPSPATH = 'mock_signups.csv'
        self.CONFIGFILEPATH = 'init_test_config.json'
        self.SERVERINFOPATH = 'mock_serverinfo.csv'
        self.EC2SECRETPATH = 'mock_ec2_secret'
        self.S3SECRETPATH = 'mock_s3_secret'
        self.MONITORPUBKEYPATH = 'MONITORKEYS.pub'

        self.MEMAIL = 'MEMAIL'
        self.MKEYINFO = 'MKEYINFO'
        self.MCUSTOMER_ID = 'cus_x14Charactersx'
        self.MSUBSCRIPTION_ID = 'sub_x14Characterx'
        self.MPLAN_ID = 'XX_consumer_iteration_#_GREEKLETTER#_2XXX-XX-XX'
        self.MSECRETSFILE = 'MSECRETSFILE'
        self.MENCODED_IDS = 'on2wex3yge2eg2dbojqwg5dfoj4a-mn2xgx3yge2eg2dbojqwg5dfojzxq'

        FilePath(self.SIGNUPSPATH).setContent('')
        FilePath(self.CONFIGFILEPATH).setContent(CONFIGFILEJSON)
        FilePath(self.SERVERINFOPATH).setContent('')
        FilePath(self.EC2SECRETPATH).setContent(MOCKEC2SECRETCONTENTS)
        FilePath(self.S3SECRETPATH).setContent(MOCKS3SECRETCONTENTS)
        FilePath(self.MONITORPUBKEYPATH).setContent(MONITORPUBKEY)

        self.DEPLOYMENT_CONFIGURATION = signup.DeploymentConfiguration(
            products=[{"description": "stuff"}],
            s3_access_key_id=ZEROPRODUCT["s3_access_key_id"],
            s3_secret_key=MOCKS3SECRETCONTENTS,
            amiimageid="ami-deadbeef", # XXX Where does this come from?  Who knows.
            instancesize="fake instance size",

            usertoken=None,
            producttoken=None,

            ssec2_access_key_id=ZEROPRODUCT["s3_access_key_id"],
            ssec2_secret_path=self.S3SECRETPATH,

            ssec2admin_keypair_name=ZEROPRODUCT["admin_keypair_name"],
            ssec2admin_privkey_path=ZEROPRODUCT["admin_privkey_path"],

            monitor_pubkey_path=ZEROPRODUCT["monitor_pubkey_path"],
            monitor_privkey_path=ZEROPRODUCT["monitor_privkey_path"],

            secretsfile=open(self.MSECRETSFILE, "a+b"),
            serverinfopath=self.SERVERINFOPATH,
        )
        self.SUBSCRIPTION = signup.SubscriptionDetails(
            bucketname="lae-" + self.MENCODED_IDS,
            oldsecrets=None,
            customer_email=self.MEMAIL,
            customer_pgpinfo=self.MKEYINFO,
            product_id=None,
            customer_id=self.MCUSTOMER_ID,
            subscription_id=self.MSUBSCRIPTION_ID,
            introducer_port_number=12345,
            storage_port_number=12346,
        )

        self.patch(signup, 'POLL_TIME', 0.1)
        self.patch(signup, 'CC_VERIFICATION_TIME', 0.3)
        self.patch(signup, 'ADDRESS_DELAY_TIME', 0.1)
        self.patch(signup, 'ADDRESS_WAIT_TIME', 0.3)
        self.patch(signup, 'LISTEN_POLL_TIME', 0.1)
        self.patch(initialize, 'SET_TAGS_DELAY_TIME', 0.1)

        from lae_automation.aws.queryapi import time
        def call_time():
            return 0
        self.patch(time, 'time', call_time)

        from lae_automation.aws import queryapi
        def call_make_http_request(request_url):
            self.failUnlessEqual(request_url, self.fakeURLs.pop(0))
            return defer.succeed(self.mhr_return_values.pop(0))
        self.patch(queryapi, 'make_http_request', call_make_http_request)

        from lae_automation.aws.queryapi import EC2ConsoleClient
        def call_EC2ConsoleClient_describe_console_output(EC2Instance, instance_id):
            return defer.succeed(
                EC2Instance.parser.describe_console_output(getconsoleoutputresponse) )
        self.patch(EC2ConsoleClient, 'describe_console_output',
                   call_EC2ConsoleClient_describe_console_output)

        # Because the S3 Client call to S3 is made through txaws, it circumvents make_http_request,
        # and necessitates a separate patch to isolate the system from remote components.
        # The patched function is the submit method of the query object in initialize.
        # This attribute belongs to the Query class object imported by devpay_s3client.
        from lae_automation.aws.devpay_s3client import Query as S3_Query
        def call_s3_query_submit(QueryObject):
            header_dict = QueryObject.get_headers()
            self.failUnlessEqual(header_dict['Date'], 'Thu, 01 Jan 1970 00:00:00 GMT')
            self.failUnlessEqual(header_dict['Content-Length'], 0)
            self.failUnlessEqual(header_dict['Authorization'],
                                 'AWS TESTS3S3S3S3S3S3S3S3:6k3/9M2RYfjOEcBm6o5p8f9oDMs=')
            self.failUnlessEqual(header_dict['Content-MD5'], '1B2M2Y8AsgTpgAmY7PhCfg==')

            return defer.succeed('Completed devpay bucket creation submission.')
        self.patch(S3_Query, 'submit', call_s3_query_submit)

        from lae_automation.initialize import EC2Client
        def call_run_instances(EC2ClientObject, ami_image_id, mininstancecount, maxinstancecount,
                               secgroups, keypair_name, instance_type):
            self.failUnlessEqual(ami_image_id, 'ami-deadbeef')
            self.failUnlessEqual(mininstancecount, 1)
            self.failUnlessEqual(maxinstancecount, 1)
            self.failUnlessEqual(secgroups, ['CustomerDefault'])
            self.failUnlessEqual(keypair_name, 'ADMINKEYS')
            class MockEC2Instance:
                def __init__(self):
                    self.launch_time = "blah"
                    self.instance_id = 'i-MOCKEC2INSTANCEID'
            return defer.succeed([MockEC2Instance()])
        self.patch(EC2Client, 'run_instances', call_run_instances)

        def call_describe_instances(EC2ClientObject, *instance_ids):
            self.failUnlessEqual(instance_ids, ('i-MOCKEC2INSTANCEID',))
            return defer.succeed( [('0.0.0.0', '0.0.0.1')] )
        self.patch(EC2Client, 'describe_instances', call_describe_instances)

        def call_get_and_store_pubkeyfp_from_keyscan(targetIP, stdout):
            return (MOCKSERVERSSHFP, MOCKHASHEDPUBKEY)
        self.patch(initialize, 'get_and_store_pubkeyfp_from_keyscan',
                   call_get_and_store_pubkeyfp_from_keyscan)

        from lae_automation.server import NotListeningError
        self.first = True
        def call_install_server(publichost, admin_privkey_path, monitor_pubkey, monitor_privkey_path,
                                stdout, stderr):
            self.failUnlessEqual(publichost, '0.0.0.0')
            self.failUnlessEqual(admin_privkey_path, 'ADMINKEYS.pem')
            if self.first:
                self.first = False
                raise NotListeningError()
        self.patch(signup, 'install_server', call_install_server)

        def call_bounce_server(deploy_config, subscription, publichost, admin_privkey_path, privatehost,
                               stdout, stderr):
            self.failUnlessEqual(publichost, '0.0.0.0')
            self.failUnlessEqual(admin_privkey_path, 'ADMINKEYS.pem')
            self.failUnlessEqual(privatehost, '0.0.0.1')
            self.failUnlessEqual(deploy_config.s3_access_key_id, 'TEST'+'S3'*8)
            self.failUnlessEqual(deploy_config.s3_secret_key, 'S3'*20)
            self.failUnlessEqual(deploy_config.bucketname, "lae-" + self.MENCODED_IDS)
            self.failUnlessEqual(deploy_config.oldsecrets, None)
            expected_suffix = 'secrets/XX_consumer_iteration_#_GREEKLETTER#_2XXX-XX-XX/1970-01-01T000000Z-%s/SSEC2' % (self.MENCODED_IDS,)
            self.assertTrue(
                deploy_config.secretsfile.name.endswith(expected_suffix),
                deploy_config.secretsfile.name)
        self.patch(signup, 'bounce_server', call_bounce_server)

        def call_send_signup_confirmation(publichost, customer_email, furl, customer_keyinfo, stdout,
                                          stderr):
            self.failUnlessEqual(publichost, '0.0.0.0')
            self.failUnlessEqual(customer_email, 'MEMAIL')
            self.failUnlessEqual(furl, None)
            self.failUnlessEqual(customer_keyinfo, 'MKEYINFO')
            return defer.succeed("Tested send confirmation email call!")
        self.patch(signup, 'send_signup_confirmation', call_send_signup_confirmation)

        def call_send_notify_failure(f, customer_email, logfilename, stdout, stderr):
            self.failUnless(isinstance(f, Failure), f)
            self.failUnlessEqual(customer_email, 'MEMAIL')
            self.failUnless(logfilename.endswith('secrets/XX_consumer_iteration_#_GREEKLETTER#_2XXX-XX-XX/1970-01-01T000000Z-%s/signup_logs' % (self.MENCODED_IDS,)), logfilename)
            return f
        self.patch(signup, 'send_notify_failure', call_send_notify_failure)

        from txaws.ec2.client import Query as EC2_Query
        def call_ec2_query_submit(QueryObject):
            return defer.succeed(createtagsresponse)
        self.patch(EC2_Query, 'submit', call_ec2_query_submit)

    def tearDown(self):
        FilePath(self.SIGNUPSPATH).remove()
        FilePath(self.CONFIGFILEPATH).remove()
        FilePath(self.SERVERINFOPATH).remove()
        FilePath(self.EC2SECRETPATH).remove()

    def initialize_testlocal_state(self, test_name):
        timestamp = '1970-01-01T00:00:00Z'
        fpcleantimestamp = timestamp.replace(':', '')
        logdirname = "%s-%s" % (fpcleantimestamp, self.MENCODED_IDS)
        testconfigdir = self.mockconfigdir.child(test_name).child('secrets').child(self.MPLAN_ID).child(logdirname)
        testconfigdir.makedirs()
        MLOGFILE_fp = FilePath(testconfigdir.path + '/signup_logs')
        MSSEC2_secretsfile = FilePath(testconfigdir.path + '/SSEC2').open('a+')
        signup_logfile = MLOGFILE_fp.open('a+')
        signup_stdout = LoggingStream(signup_logfile, '>')
        signup_stderr = LoggingStream(signup_logfile, '')
        return signup_stdout, signup_stderr, MLOGFILE_fp.path, MSSEC2_secretsfile

    def test_activate_subscribed_service(self):
        stdout, stderr, MLOGFILENAME, MSSEC2SECRETSFILE = self.initialize_testlocal_state('test_activate_subscribed_service')
        self.patch(signup, 'VERIFY_POLL_TIME', .1)
        self.patch(signup, 'VERIFY_TOTAL_WAIT', .2)

        from lae_automation.aws import queryapi
        def call_hostpubkeyextractor(consoletext, instanceId):
            return MOCKSERVERSSHFP
        self.patch(queryapi, 'hostpubkeyextractor', call_hostpubkeyextractor)

        config = attr.assoc(
            self.DEPLOYMENT_CONFIGURATION,
            secretsfile=MSSEC2SECRETSFILE,
        )
        attr.validate(config)
        d = signup.activate_subscribed_service(
            config,
            self.SUBSCRIPTION,
            stdout, stderr,
            MLOGFILENAME,
        )

        def _check(ign):
            content = FilePath(MLOGFILENAME).getContent()
            self.failUnlessIn('MEMAIL', content)
            self.failUnlessIn('MKEYINFO', content)
        d.addCallback(_check)
        return d

    def test_no_products(self):
        invalid = attr.asdict(self.DEPLOYMENT_CONFIGURATION)
        invalid["products"] = []
        self.assertRaises(
            ValueError,
            signup.DeploymentConfiguration,
            **invalid
        )

    def test_timeout_addressreq(self):
        stdout, stderr, MLOGFILENAME, MSSEC2_secretsfile = self.initialize_testlocal_state('test_timeout_addressreq')
        from lae_automation.aws import queryapi
        def call_get_EC2_properties(ec2accesskeyid, ec2secretkey, EC2_ENDPOINT, parser,
                                    *instance_ids):
            return defer.succeed(None)
        self.patch(queryapi, 'get_EC2_properties', call_get_EC2_properties)

        config = attr.assoc(
            self.DEPLOYMENT_CONFIGURATION,
            secretsfile=MSSEC2_secretsfile,
        )
        attr.validate(config)
        d = signup.activate_subscribed_service(config, self.SUBSCRIPTION, stdout, stderr, MLOGFILENAME)
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(signup.TimeoutError)
            stdout.close()
            logfp = FilePath(MLOGFILENAME)
            out = logfp.getContent()
            self.failUnlessIn("Timed out", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_EC2_not_listening(self):
        stdout, stderr, MLOGFILENAME, MSSEC2_secretsfile = self.initialize_testlocal_state('test_EC2_not_listening')
        self.patch(signup, 'VERIFY_POLL_TIME', .1)
        self.patch(signup, 'VERIFY_TOTAL_WAIT', .2)

        from lae_automation.aws import queryapi
        def call_get_EC2_consoleoutput(ec2accesskeyid, ec2secretkey, endpoint_uri, instance_id):
            return defer.succeed(None)
        self.patch(queryapi, 'get_EC2_consoleoutput', call_get_EC2_consoleoutput)

        database_path = FilePath(self.mktemp().decode("utf-8"))
        database_path.makedirs()
        smclient = memory_client(database_path)
        
        config = attr.assoc(
            self.DEPLOYMENT_CONFIGURATION,
            secretsfile=MSSEC2_secretsfile,
        )
        attr.validate(config)
        d = signup.activate_subscribed_service(
            config, self.SUBSCRIPTION, stdout, stderr, MLOGFILENAME,
            smclient,
        )
        def _bad_success(ign):
            self.fail("should have got a failure")
        def _check_failure(f):
            f.trap(signup.TimeoutError)
            stdout.close()
            logfp = FilePath(MLOGFILENAME)
            out = logfp.getContent()
            self.failUnlessIn("Timed out", out)
        d.addCallbacks(_bad_success, _check_failure)
        return d

    def test_get_bucket_name(self):
        self.failUnlessEqual(b32encode("abc"), "MFRGG===")
        self.failUnlessEqual(b32encode("def"), "MRSWM===")
        self.failUnlessEqual(signup.get_bucket_name("abc", "def"), "lae-mfrgg-mrswm")


TestSignupModule.test_activate_subscribed_service.__func__.skip = "mostly obsolete"
