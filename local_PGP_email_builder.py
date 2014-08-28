#To be run in mf/confirmation/$TARGET

import subprocess, os, sys, glob, simplejson

from urllib import unquote
from twisted.python.filepath import FilePath

from lae_automation.confirmation import CONFIRMATION_EMAIL_BODY

def extract_logs_from_tarball(archive_name):
    extraction_arg_string = 'tar -xjvf %s' % archive_name
    extraction_arg_list = extraction_arg_string.split()
    subprocess.call(extraction_arg_list)
    path_pattern = 'home/website/secrets/S4_consumer_iteration_2_beta1_2014-05-27/*/*'
    for path in glob.glob(path_pattern):
        cp_arg_string = 'cp %s ./' % path
        cp_arg_list = cp_arg_string.split()
        cp_sp = subprocess.call(cp_arg_list)
    return True

def extract_PGP_key(work_dir_path):
    stripe_list = simplejson.load(open(work_dir_path+'/stripe','r'))
    PGP_pubkey = stripe_list[1]
    PGP_pubkey = unquote(PGP_pubkey)
    FilePath('PGP_pubkey.asc').setContent(PGP_pubkey)
    return True

def import_PGP_key(PGP_file_path):
    gpg_import_call_string = 'gpg --import PGP_pubkey.asc'
    gpg_import_call_list = gpg_import_call_string.split()
    import_errput = open('gpg_import_err.txt', 'w')
    sp = subprocess.Popen(gpg_import_call_list, stderr = import_errput)
    sp.wait()
    outstring = FilePath('gpg_import_err.txt').getContent()
    ID = outstring.lstrip('gpg: key ')[:8]
    return ID

def extract_furl():
    signup_log = FilePath('signup_logs').getContent()
    log_lines = signup_log.split('\n')
    introducer_furl = [(line.strip()).lstrip('introducer.furl = ') for line in log_lines if line.startswith('introducer.furl = pb://')][0]
    return introducer_furl

def create_confirmation_email(introducer_furl):
    email_content = CONFIRMATION_EMAIL_BODY % {'external_introducer_furl':introducer_furl}
    FilePath('confirmation.txt').setContent(email_content)

def encrypt_sign_confirmation(ID):
    enc_string = 'gpg -seaR %s -R zancas@leastauthority.com -o msg.asc confirmation.txt' % ID
    enc_list = enc_string.split()
    sp = subprocess.Popen(enc_list)
    sp.wait()

if __name__ == '__main__':
    signup_email = sys.argv[1]
    work_dir_path = '/home/arc/mf/confirmations/%s/' % signup_email
    os.chdir(work_dir_path)
    archive_name = '%s_signup_PGP_data.tar.bz2' % signup_email
    if extract_logs_from_tarball(archive_name):
        if extract_PGP_key(work_dir_path):
            ID = import_PGP_key('PGP_pubkey.asc')
            intro_furl = extract_furl()
            create_confirmation_email(intro_furl)
            encrypt_sign_confirmation(ID)
    else:
        print "Failed to extract logs"
        sys.exit(-1)
