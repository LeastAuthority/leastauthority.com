MOCKJSONCONFIGFILE = """{
  "products": [
    { "amount":                  "2500",
      "interval":                "month",
      "currency":                "USD",
      "full_name":               "Secure Simple Storage Service test S4",
      "plan_ID":                 "XX_consumer_iteration_#_GREEKLETTER#_2XXX-XX-XX",
      "plan_name":               "S4",
      "plan_trial_period_days":  "30",
      "ami_image_id":            "ami-deadbeef",
      "instance_size":           "t1.micro"
    }
  ],
  "ssec2admin_keypair_name":  "ADMINKEYS",
  "ssec2admin_privkey_path":  "ADMINKEYS.pem",
  "s3_access_key_id":         "TESTS3S3S3S3S3S3S3S3",
  "s3_secret_path":           "mock_s3_secret",
  "ssec2_access_key_id":      "TESTEC2EC2EC2EC2EC2E",
  "ssec2_secret_path":        "mock_ec2_secret",

  "monitor_pubkey_path":    "MONITORKEYS.pub",
  "monitor_privkey_path":   "MONITORKEYS.pem",
  "incident_gatherer_furl": "MOCK_incident_gatherer_furl",
  "stats_gatherer_furl":    "MOCK_stats_gatherer_furl",
  "sinkname_suffix":        "unitteststorageserver/rss"
}"""
