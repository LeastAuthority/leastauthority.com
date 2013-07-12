#!/bin/sh
cd /home/website/leastauthority.com
flappserver restart `pwd`/flapp
sleep 2
PYTHONPATH=. authbind --deep sh -c 'python -u `pwd`/lae_site/main.py "$@" >>../site.out 2>&1'
