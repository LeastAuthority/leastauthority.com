#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u"Zooko Wilcox-O'Hearn"
SITENAME = u'Least Authority Blog'
SITEURL = ''

TIMEZONE = 'America/Los_Angeles'

DEFAULT_LANG = u'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

# Blogroll
LINKS =  (('Pelican', 'http://getpelican.com/'),
          ('Python.org', 'http://python.org/'),
          ('Jinja2', 'http://jinja.pocoo.org/'),
          ('You can modify those links in your config file', '#'),)

# Social widget
SOCIAL = (('You can add links in your config file', '#'),
          ('Another social link', '#'),)

DEFAULT_PAGINATION = 5
DISPLAY_CATEGORIES_ON_MENU = True

# Uncomment following line if you want document-relative URLs when developing
RELATIVE_URLS = True

THEME = "themes/lae"
DELETE_OUTPUT_DIRECTORY = True
OUTPUT_PATH = '../lae_site/templates/blog/'

FEED_DOMAIN = SITEURL