#!/bin/sh

export PYTHONPATH=.

timeout --kill-after=121m 2h \
    python ./full_signup.py \
    --log-directory ../secrets/flappserver_logs/ \
    --secrets-directory ../secrets/ \
    --automation-config-path ../secret_config/lae_automation_config.json \
    --server-info-path ../serverinfo.csv
