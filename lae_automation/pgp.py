#To be run in $(SERVICE_ID)/confirmations/$(SUBSCRIBER_EMAIL_ADDRESS)
#See Usage messages in main().

import subprocess, os, sys, glob, simplejson

from urllib import unquote
from twisted.python.filepath import FilePath

from lae_automation.confirmation import CONFIRMATION_EMAIL_BODY

def extract_logs_from_tarball(archive_name, service_id):
    extraction_arg_list = ['tar', '-xjvf', archive_name]
    subprocess.call(extraction_arg_list)
    path_pattern = 'home/website/secrets/'+service_id+'/*/*'
    for path in glob.glob(path_pattern):
        cp_arg_list = ['cp', str(path), './']
        subprocess.call(cp_arg_list)
    return True

def extract_PGP_key(work_dir_path):
    stripe_list = simplejson.loads(work_dir_path.child('stripe').getContent())
    PGP_pubkey = stripe_list[1]
    PGP_pubkey = unquote(PGP_pubkey)
    FilePath('PGP_pubkey.asc').setContent(PGP_pubkey)
    return True

def import_PGP_key(PGP_file_path):
    gpg_import_call_list = ['gpg', '--import', 'PGP_pubkey.asc']
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

def encrypt_sign_confirmation(ID, PGP_contact_email):
    print "Attempting to generate ascii armored cyphertext in 'msg.asc'."
    enc_list = ['gpg', '-seaR', str(ID), '-R', PGP_contact_email, '-o', 'msg.asc', 'confirmation.txt']
    sp = subprocess.Popen(enc_list)
    sp.wait()
