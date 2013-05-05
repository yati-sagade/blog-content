Installing MediaWiki locally
##############################################
:date: 2012-11-15
:title: Installing MediaWiki locally
:author: Yati Sagade
:tags: FOSS, MediaWiki, Wikipedia
:category: FOSS
:slug: installing-mediawiki
:summary: A log of how I installed MediaWiki, the Wiki that powers Wikipedia.

I am a complete newbie to Wikimedia-land. The first thing that evidently
tripped me was the names. They seemed to be random permutations of the words 
"Wiki", "Media" and "Pedia". `This page`_ cleared all the confusion. 

The installation of MediaWiki is pretty straightforward. I am using Apache and
MySQL to power my wiki, but there are options to use other webservers and 
databases.

MediaWiki is available for `download here`_. It is also possible to clone the git 
repository(more info `here <http://www.mediawiki.org/wiki/Download_from_Git>`_).

Once downloaded, the simplest way to install MediaWiki is to extract the 
tarball into the httpd(Apache) public folder - commonly called the document
root or the docroot. For me, this is ``/var/www/html``. 

.. code-block:: text

    # tar xzv </home/yati/Downloads/mediawiki-*.tar.gz -C /var/www/html/
    # mv /var/www/html/mediawiki-* mediawiki

I run Fedora, and the PHP installation by default does not log any error 
information. I changed this because I was experiencing some problems with the
MediaWiki installation. This can be done by editing the ``php.ini`` file, which 
is ``/etc/php.ini`` for me. 

.. code-block:: text

    error_reporting = E_ALL | E_STRICT
    
    error_log = syslog

Apache should be restarted after editing ``php.ini``

.. code-block:: text

    # apachectl -k restart

This allowed me to troubleshoot the installation by just ``tail``ing 
``/var/log/messages``. Next, pointing the browser to 
``http://localhost/mediawiki`` brought up a nice picture of a flower and a link 
to start setting up the wiki. However, clicking on that link got me a blank 
page. Looking in the logs, I found the reason

.. code-block:: text

    # tail /var/log/messages
    ...
    Nov 15 22:57:26 localhost httpd: PHP Fatal error:  Class 'DOMDocument' not found in /var/www/html/mediawiki/includes/LocalisationCache.php on line 542

Someone in #mediawiki on Freenode_ pointed out that I am missing the PHP
DOM extension that allows PHP developers to hack the Document Object Model(DOM)
from within PHP. This_ is the extension page with detailed instructions on 
installation and configuration on different operating systems. On Fedora, this
is how that extension is installed:

.. code-block:: text

    # yum -y install php-xml

After this, to allow the automatic loading of this extension when PHP starts, 
the ``php.ini`` file needs to be edited to include the following line:

.. code-block:: text

    extension = dom.so

After restarting Apache 

.. _`This page`: http://www.mediawiki.org/wiki/Wikipmediawiki
.. _`download here`: http://www.mediawiki.org/wiki/Download
.. _Freenode: irc://irc.freenode.net
.. _This: http://ru2.php.net/manual/en/intro.dom.php
