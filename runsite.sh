#!/bin/sh

set -e
cd $(readlink -f $(dirname $0))
flappserver restart `pwd`/flapp
sleep 2
cmd="/home/arc/LeastAuthority_env/bin/python -u `pwd`/lae_site/main.py"
pids=$(pgrep -fl "$cmd" |cut -f1 -d' ')
if [ "$pids" != "" ]; then
       echo killing $pids
       kill $pids
fi
sleep .1
if [ "$1" = "--dev" ]; then
    PYTHONPATH=. $cmd --dev
else
    PYTHONPATH=. authbind --deep sh -c "$cmd"' "$@" >>../site.out 2>&1' &
fi
sleep .1
pgrep -fl "$cmd"
