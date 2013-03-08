#!/bin/sh
cd leastauthority.com
flappserver restart `pwd`/flapp
sleep 2
PYTHONPATH=. authbind --deep python `pwd`/lae_site/main.py "$@"
