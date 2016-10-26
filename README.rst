leastauthority.com
==================

.. image:: https://travis-ci.org/LeastAuthority/leastauthority.com.svg?master
    :target: https://travis-ci.org/LeastAuthority/leastauthority.com

This website is a twisted web server with a set of static files.
In addition it has some web-application logic for interacting with
Amazon's services (AWS), such as devpay, ec2, and s3 on behalf of LAE
customers.


Preparatory Configuration
=========================

For this website to function correcly, Amazon Web Services (AWS) and
SSL certificates must also be correctly configured.

Configuring a correct deployment requires these steps:

0. FIXME: This procedure will be changing rapidly as we develop the site.  Please remember to update this list.
1. Sign up for Amazon devpay: http://aws.amazon.com/devpay/
  1. Enter the configuration necessary for the LAE product line.  This directly affects customer facing details and revenue flow.
  2. You will be asked for the "Product Redirect URL".  That will be "https://${deployment_hostname}/devpay-complete".  FIXME: URL handler is not implemented;  FIXME: https not yet configured.
  3. Enter the product code, product token, and purchase url in the configuration file as described below.  FIXME: Maybe all these do not need to be in the config file.  FIXME: only purchase url is currently implemented.
2. Configure SSL certificates.  FIXME: This is not implemented.
3. FIXME: Not implemented: configure the ec2 and s3 goo to auto-deploy tahoe services.


Initial Webserver Deployment
============================

XXX Should break this into two sections: Deploy EC2 and Deploy Webserver

1. Deploy the correct ec2 image.
2. Checkout the website repository into the correct place.
   The repo is at:
         - 'leastauthority.com:/home/website/leastauthority.com'
   By default /home/website is the parent repo.  This directory contains:
         - leastauthority.com/:  The parent directory of the server code
         - signup.furl:   The furl for the flappserver signup service
         - lae_site_config.json: Contains the available product info.
         - lae_automation_config.json:  Contains product secrets, credential paths.
3. Install the correct dependencies.
         - foolscap:  for the flappserver
         - jinja2:    handles webserver templates         
4. Set up the correct website user account.
         - 'website'
5. Checkout the website configuration repository for the target deployment environment.
         - 'leastauthority.com:/home/website'	 
6. Run the webserver.
         - You can do this by modifying: 'setuplocalserver.sh' to your system.  XXX Obviously much remains to be done here.
7. Ensure the instance has a static IP address.
8. Test the website in your browser by its IP address.
9. Update the DNS entry for the website host to point to the new IP address.


Configuration File
==================

This website requires a configuration file in the home directory of the
user which runs the webserver, named "lae_website_config.json" which is
in JSON format.  It must define these keys:

"purchase_url" :
This should be the "purchase URL" found on the devpay site after you
have set up a particular application and billing configuration.


Upgrade
=======

Follow the same steps as for "initial deployment", except you will have
a new instance with a new IP.  In the last step, you will cause new DNS
requests to target the new server instance.

Wait the appropriate amount of time before taking down the old instance
because remote browsers may be relying on cached DNS entries.
