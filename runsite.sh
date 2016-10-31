#!/bin/sh

set -e
cd $(readlink -f $(dirname $0))
flappserver restart `pwd`/flapp
sleep 2

SECRETS="${PWD}/../secret_config"
LOGS="${PWD}/.."

cmd="\
python -u ${PWD}/lae_site/main.py \
\
--signup-furl-path ${SECRETS}/signup.furl \
--stripe-api-key-path ${SECRETS}/stripeapikey \
\
--site-logs-path ${LOGS}/sitelogs \
--interest-path ${LOGS}/emails.csv \
--subscriptions-path ${LOGS}/subscriptions.csv \
--service-confirmed-path ${LOGS}/service_confirmed.csv \
\
--port 8443 \
--redirectport 8080 \

"
pattern="python -u `pwd`/lae_site/main.py"
pids=$(pgrep -fl "$pattern" |cut -f1 -d' ')
if [ "$pids" != "" ]; then
       echo killing $pids
       kill $pids
fi
sleep .1
if [ "$1" = "--dev" ]; then
    cmd2="$cmd --dev"
    echo "$cmd2"
    PYTHONPATH=. sh -c "$cmd2" 2>&1
else
    cmd2="$cmd $@"
    echo "$cmd2"
    PYTHONPATH=. authbind --deep sh -c "$cmd2" >>../site.out 2>&1 &
fi
sleep .1
pgrep -fl "$cmd"
