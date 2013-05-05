#!/usr/bin/env python
# -*- coding: utf-8 -*- #
import sys
# The pelicanplugins package is in ..
sys.path.append('..')
from pelicanplugins import latex

PLUGINS = [latex,]


AUTHOR = u'Yati Sagade'
SITENAME = u'explog'
SITEURL = ''

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = u'en'

THEME = '/home/yati/Projects/BlogPosts/explog-theme'

# Blogroll
LINKS =  (('Pelican', 'http://docs.notmyidea.org/alexis/pelican/'),
          ('Python.org', 'http://python.org'),
          ('Jinja2', 'http://jinja.pocoo.org'),
          ('You can modify those links in your config file', '#'),)

# Social widget
SOCIAL = (('twitter', 'http://www.twitter.com/yati_itay'),
          ('github', 'http://www.github.com/yati-sagade'),
          ('google-plus', 'https://plus.google.com/102607933018279616737'),)



DEFAULT_PAGINATION = False

