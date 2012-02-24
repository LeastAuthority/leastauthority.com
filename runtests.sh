#!/bin/sh
python check-miscaptures.py
echo
pyflakes .
echo
trial --rterrors lae_site lae_util lae_automation
