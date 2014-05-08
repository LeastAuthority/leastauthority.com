#!/bin/sh

set -e
cd $(readlink -f $(dirname $0))
flappserver restart `pwd`/flapp
sleep 2
cmd="python -u '`pwd`/lae_site/main.py'"
pattern="python -u `pwd`/lae_site/main.py"
pids=$(pgrep -fl "$pattern" |cut -f1 -d' ')
if [ "$pids" != "" ]; then
       echo killing $pids
       kill $pids
fi
sleep .1
if [ "$1" = "--dev" ]; then
    cmd2="$cmd --dev 2>&1"
    echo "$cmd2"
    PYTHONPATH=. sh -c "$cmd2"
else
    cmd2="$cmd $@"
    echo "$cmd2"
    PYTHONPATH=. authbind --deep sh -c "$cmd2" >>../site.out 2>&1 &
fi
sleep .1
pgrep -fl "$cmd"
