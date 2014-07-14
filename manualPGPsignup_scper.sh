#! /bin/bash

localdir=~/mf/confirmations/$1
mkdir $localdir
scp website@leastauthority.com:"/home/website/secrets/manualPGPtarballs/$1_signup_PGP_data.tar.bz2" "$localdir"/