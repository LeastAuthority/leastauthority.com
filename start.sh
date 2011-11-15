#!/bin/sh
BASE=/home/arc/pyvirtenvs/website/leastauthority.com/
cd ${BASE}
PYTHONPATH=${BASE} authbind --deep python ./lae_site/main.py "$@"
