#!/bin/sh

export PYTHONPATH=.

timeout --kill-after=121m 2h \
    ./full_signup.py \
    --log-directory /app/data/secrets/flappserver_logs/ \
    --secrets-directory /app/data/secrets/ \
    --automation-config-path /app/secret_config/lae_automation_config.json \
    --server-info-path /app/data/serverinfo.csv
