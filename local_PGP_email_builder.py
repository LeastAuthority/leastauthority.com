#To be run in $(SERVICE_ID)/confirmations/$(SUBSCRIBER_EMAIL_ADDRESS)
#See Usage messages in main().

import os, sys

from lae_automation.pgp import extract_logs_from_tarball, extract_PGP_key, import_PGP_key,\
                               extract_furl, create_confirmation_email, encrypt_sign_confirmation

def main():
    if len(sys.argv) != 4:
        print "Usage: python $(leastauthority.com_DIR)/local_PGP_email_builder.py service_id email_of_subscriber email_of_LA_PGP_handler"
        sys.exit(-10)
    service_id = sys.argv[1]
    signup_email = sys.argv[2]
    encryption_confirmation_handler_email = sys.argv[3]
    work_dir_path = os.getcwd()
    if (os.path.basename(work_dir_path) != signup_email) or not (os.path.dirname(os.getcwd()).endswith('confirmations')):
        print "This script must be run in the PGP-signup confirmation-email directory for the relevant service, and signup: "
        print "e.g. S4_EXAMPLE_SERVICE/confirmations/foo@spam.net/"
        print "instead it was run in: ' "+os.getcwd()+" '"
        sys.exit(-9)
    os.chdir(work_dir_path)
    archive_name = '%s_signup_PGP_data.tar.bz2' % signup_email
    if extract_logs_from_tarball(archive_name, service_id):
        if extract_PGP_key(work_dir_path):
            ID = import_PGP_key('PGP_pubkey.asc')
            intro_furl = extract_furl()
            create_confirmation_email(intro_furl)
            encrypt_sign_confirmation(ID, encryption_confirmation_handler_email)
    else:
        print "Failed to extract logs"
        sys.exit(-1)

if __name__ == '__main__':
    main()
