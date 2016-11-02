#!/bin/sh

export PYTHONPATH=.

timeout --kill-after=121m 2h \
    ./full_signup.py \
    --log-directory /app/data/secrets/flappserver_logs/ \
    --secrets-directory /app/data/secrets/
