#!/bin/sh
BASE=/home/website/leastauthority.com
cd ${BASE}
PYTHONPATH=. authbind --deep python ./lae_site/main.py "$@"
