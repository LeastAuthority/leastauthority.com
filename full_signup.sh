#!/bin/sh

export PYTHONPATH=.

timeout --kill-after=121m 2h \
    python ./full_signup.py \
    --log-directory ../secrets/flappserver_logs/ \
    --secrets-directory ../secrets/
