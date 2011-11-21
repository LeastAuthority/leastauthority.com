#!/bin/sh
#BASE=/home/arc/pyvirtenvs/website/leastauthority.com/
#cd ${BASE}
PYTHONPATH=. authbind --deep python ./lae_site/main.py "$@"
