
from txacme.urls import LETSENCRYPT_DIRECTORY, LETSENCRYPT_STAGING_DIRECTORY

from lae_util.k8s_cert_store import LEK8SParser

lek8s_parser = LEK8SPARSER("le-k8s", LETSENCRYPT_DIRECTORY)
letsk8s_parser = LEK8SPARSER("lets-k8s", LETSENCRYPT_STAGING_DIRECTORY)
