#!/bin/sh
flappserver restart flapp
sleep 2
PYTHONPATH=. authbind --deep python ./lae_site/main.py "$@"
