#!/bin/sh
set -eu

rootdir="$HOME/secrets/S4_consumer_iteration_2_beta1_2014-05-27"
pwd=$(pwd)

cd "$rootdir"
dir=$(grep -nR "$1" * | cut -d'/' -f1)
cust_ssec2="$rootdir"/"$dir"/SSEC2
cust_stripe="$rootdir"/"$dir"/stripe

# echo "$dir"
cat $cust_ssec2 | python -c 'import sys, json; print json.load(sys.stdin)["publichost"]'
cd "$pwd"

