#!/bin/sh
trial --rterrors lae_site lae_util lae_automation
python check-miscaptures.py
echo
pyflakes .
echo
