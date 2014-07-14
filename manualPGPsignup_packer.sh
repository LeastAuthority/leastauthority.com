#! /bin/bash

targets=`grep -l -r -i $1 ../secrets/S4_consumer_iteration_2_beta1_2014-05-27/`
tar -cjvf "../secrets/manualPGPtarballs/$1_signup_PGP_data.tar.bz2" $targets
