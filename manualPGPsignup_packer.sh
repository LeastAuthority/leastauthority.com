#! /bin/bash

target=`grep -l -r -i "$1" /home/website/secrets/S4_consumer_iteration_2_beta1_2014-05-27/ | head -n 1`
target_directory=`dirname $target`
tar -cjvf "/home/website/secrets/manualPGPtarballs/$1_signup_PGP_data.tar.bz2" $target_directory/*
