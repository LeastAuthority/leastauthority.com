#!/bin/sh
trial --rterrors lae_site lae_util lae_automation
python2.7 check-miscaptures.py
echo
pyflakes .
echo
