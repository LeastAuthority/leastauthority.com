#!/bin/sh

export PYTHONPATH=.

timeout --kill-after=121m 2h \
    ./full_signup.py \
    --log-directory /app/data/secrets/flappserver_logs/ \
    --secrets-directory /app/data/secrets/ \
    --automation-config-path /app/k8s_secrets/config-lae-automation.json \
    --server-info-path /app/data/serverinfo.csv \
    --domain "$(cat /app/k8s_secrets/domain)" \
    --subscription-manager-endpoint "http://127.0.0.1:8000/"
