#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u"Zooko Wilcox-O'Hearn"
SITENAME = u'Least Authority Blog'
SITEURL = 'https://leastauthority.com/blog'

TIMEZONE = 'UTC'

DEFAULT_LANG = u'en'
FEED_ALL_ATOM = 'feeds/all.atom.xml'
CATEGORY_FEED_ATOM = 'feeds/%s.atom.xml'
DELETE_OUTPUT_DIRECTORY = True

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
OUTPUT_PATH = '../content/blog/'

FEED_DOMAIN = SITEURL

DISQUS_SITENAME = 'leastauthority'

READERS = {"html": None}

