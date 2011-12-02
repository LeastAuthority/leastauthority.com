
CONFIRMATION_EMAIL_SUBJECT = "Your sign-up to Tahoe-LAFS-on-S3 is complete"

CONFIRMATION_EMAIL_TEXT = """
Welcome to the alpha programme!  Please go to

  https://leastauthority.com/howtoconfigure

for instructions on how to set up and configure your Tahoe-LAFS gateway
and start using the service.

The settings for your gateway's tahoe.cfg are:


[client]
introducer.furl = %(external_introducer_furl)s

shares.needed = 1
shares.happy = 1
shares.total = 1

[storage]
enabled = false


(The introducer.furl setting should be on one line.)

We hope you enjoy using our service and find it useful.

--\x20
The Least Authority Enterprises team
"""