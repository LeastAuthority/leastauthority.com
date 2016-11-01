#!/bin/sh
cd /home/website/leastauthority.com
PYTHONPATH=. timeout --kill-after=121m 2h python ./full_signup.py
