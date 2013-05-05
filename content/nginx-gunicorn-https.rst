:Title: Configuring an HTTPS site with Django on Nginx + Gunicorn
:Date: 2012-05-11
:Tags: nginx, django, python, gunicorn
:Author: Yati Sagade
:Slug: nginx-gunicorn-https 

I have deployed quite a few Django powered sites(or projects, if you will) on PaaS like `OpenShift`_, `Google AppEngine`_ and most of the time, personal projects are best hosted on a PaaS (I'm a big PaaS fan). But I was required to deploy my final semester project on our college server. Admittedly, deployment is the single most diffcult task for newcomers. I scoured the Web, spoke to guys on IRC and finally was able to deploy Django on `Nginx`_ and `Gunicorn`_ over HTTPS (my site uses HTTPS throughout).

Step 0x00: The dummy project
--------------------------------

Create the virtualenv for our project:

.. code-block:: text

    $ mkdir dummy
    $ virtualenv --no-site-packages dummy
    New python executable in dummy/bin/python
    Installing setuptools............done.
    Installing pip...............done.
    $ source ./dummy/bin/activate
    (dummy)$ pip install django

So now we're working in the virtualenv and have the latest stable Django installed. Switch to your projects directory and start a new Django project.

.. code-block:: text

    (dummy)$ django-admin.py startproject dummy_project
    (dummy)$ cd dummy_project
    (dummy)$ ls
    manage.py dummy_project

Now test if this is working fine in Django's development server:

.. code-block:: text
    
    (dummy)$ python manage.py runserver

and go to ``http://localhost:8000/`` in your browser. You should see the Django welcome page.
Stop the development server for now and let's move on to the next step.

Step 0x01: Installing Gunicorn
--------------------------------

Gunicorn 'Green Unicorn' is a Python WSGI HTTP Server for UNIX. It is lightweight and supports Django out-of-the-box. To install, simply do

.. code-block:: text

    (dummy)$ pip install gunicorn

This will install Gunicorn in the current virtualenv (dummy here). To test if Gunicorn runs fine, 

.. code-block:: text

    (dummy)$ cd /path/to/project/dir
    (dummy)$ gunicorn_django

You should see some output on the terminal. Point your browser to ``http://localhost:8000/`` again to see if it works. (You did shut down the previously started development server, right?)

Now for deployment on a real production system, we'll need to pass a couple more options to gunicorn. Let's create a file called deploy.sh that contains the appropriate invocation for gunicorn

.. code-block:: bash

      #!/bin/bash
      set -e
      LOGFILE=/path/to/logdir/dummy.log
      LOGDIR=$(dirname $LOGFILE)
      # Number of worker processes. 
      # Should be no less than the number of cores available and a popular formula
      # is 1 + 2 * number of cores. 
      NUM_WORKERS=3
      
      USER=youruser
      
      GROUP=youruser
      
      cd /path/to/project/dir
      
      # Activate the virtualenv - replace the path with the path to wherever your venv lives.
      source /path/to/virtualenv/dummy/bin/activate
      
      test -d $LOGDIR || mkdir -p $LOGDIR
      
      # Finally, the invocation
      gunicorn_django -w $NUM_WORKERS \
          --user=$USER --group=$GROUP --log-level=debug \
          --log-file=$LOGFILE 2&gt;&gt;$LOGFILE

Now make this file executable by doing
    
.. code-block:: text

    (dummy)$ chmod a+x deploy.sh

At this point, simply executing deploy.sh should bring up gunicorn and allow us to view the welcome page on ``http://localhost:8000/``

Step 0x02: Installing and configuring Supervisor
---------------------------------------------------

`Supervisor`_ is a client/server system that allows its users to monitor and control a number of processes on UNIX-like operating systems. Why are we using it? Because it gives us control over our processes and we're able to configure things like log file paths in a central place - the supervisor configuration file. Also, adding a new project is as easy as a new block in the configuration file. Installing is super-easy

.. code-block:: text

    (dummy)$ pip install supervisor

Supervisor has two components - the server(called ``supervisord``), which daemonizes when run and the client tool(called ``supervisorctl``), which is used to start/stop our programs. Let's write the configuration file for supervisord.

Place the following in the ``etc`` directory in your virtualenv - e.g., ``/path/to/venv/dummy/etc/`` as ``supervisord.conf``

.. code-block:: text

      [program:dummy]
      directory = /path/to/project/dir
      user = youruser
      command = /path/to/deploy.sh
      stdout_logfile = /path/to/dummy.log
      stderr_logfile = /path/to/dummy.log

      [unix_http_server]
      file=/tmp/supervisor.sock   ; (the path to the socket file)
    
      [supervisord]
      logfile=/tmp/supervisord.log ; (main log file;default $CWD/supervisord.log)
      logfile_maxbytes=50MB        ; (max main logfile bytes b4 rotation;default 50MB)
      logfile_backups=10           ; (num of main logfile rotation backups;default 10)
      loglevel=info                ; (log level;default info; others: debug,warn,trace)
      pidfile=/tmp/supervisord.pid ; (supervisord pidfile;default supervisord.pid)
      nodaemon=false               ; (start in foreground if true;default false)
      minfds=1024                  ; (min. avail startup file descriptors;default 1024)
      minprocs=200                 ; (min. avail process descriptors;default 200)
   
      [rpcinterface:supervisor]
      supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface

      [supervisorctl]
      serverurl=unix:///tmp/supervisor.sock ; use a unix:// URL  for a unix socket

Be very sure to change the parameters according to your settings. Once the conf file is written, we can start ``supervisord`` as root (remember, it needs to daemonize). You will need to ``su`` to root and then activate the ``dummy`` virtualenv. Then do

.. code-block:: text

    (dummy)# supervisord

At this point, you may want to read up on the official `Supervisor`_ docs.

Okay so let us test Supervisor. As root (and in the ``dummy`` virtualenv), do

.. code-block:: text

    (dummy)# supervisorctl start dummy

Then check with your browser. To stop, 

.. code-block:: text

    (dummy)# supervisorctl stop dummy

Note that running this as root does not expose any security threats, as supervisorctl runs the actual process as the user indicated in the config file. So unless *that* is set to root, you're okay.
After ensuring supervisor is running fine, move on to the next step:

Step 0x03: Installing and configuring Nginx
-----------------------------------------------

nginx [engine x] is an HTTP and reverse proxy server, as well as a mail proxy server, written by Igor Sysoev. It is used widely to serve static files, but we'll also use it as a reverse proxy for our upstream gunicorn server. Basically, nginx will handle all real requests from the outside world, and delegate these to the gunicorn server running on port 8000 (or whichever port you decide to run it on). Requests for static files will be directly served by nginx. 

To install nginx on Fedora or RedHat, 

.. code-block:: text

    # yum install nginx

On Debian/Ubuntu, just use ``apt-get`` instead of ``yum``

Once installed, let's configure Nginx. You should read up on the official Nginx docs to know how to configure vhosts, but to keep it simple here, let's modify the default config file which, on my Fedora box, sits at ``/etc/nginx/conf.d/default.conf``.  I wanted all areas of my site SSL protected. If you have an area in your site that does not *need* to have SSL/TLS, please do not use it - it is unnecessary hard work for the server. But if you need it, do not shy away.

To configure SSL, we first need to have our SSL certificate - which can be later signed by a renowned certificate authority(CA) like Verisign. First install OpenSSL on your system

.. code-block:: text

    # yum install openssl

Then, change to a directory where you want to store your certificate - note that while this directory must be readable by Nginx, it must not be readable by all users, as it will contain the server's private key. Once you're in the directory, say  ``/path/to/cert/``

Create the private key:

.. code-block:: text

    $ openssl genrsa -out dummy-key.pem 1024

Create the CSR - Certificate Signing Request file(sent to CAs to sign our certificate)

.. code-block:: text

    $ openssl req -new -key dummy-key.pem -out dummy-csr.pem

We're not sending this CSR over to a CA now. Instead, we'll "self-sign" it:
    
.. code-block:: text

    $ openssl x509 -req -in dummy-csr.pem -signkey dummy-key.pem -out dummy-cert.pem

This generates the certificate in ``dummy-cert.pem``

Now to configure Nginx.
Open up ``/etc/nginx/conf.d/default.conf`` (on Fedora) and edit it as follows:

.. code-block:: text

    #
    # The default server
    #
    server {
        listen 80;
        # If you want certain Non-SSL areas on your site, add a location block here
        # read up on the nginx docs.
        # Be sure to replace localhost in the following rule to the server name/IP address.
        return 301 https://localhost/;
    }
    server {
        listen  443 ssl;
        # server_name  _;
        # start mine
        ssl on;
        ssl_certificate /path/to/cert/dummy-cert.pem;
        ssl_certificate_key /path/to/cert/dummy-key.pem;
        ssl_protocols        SSLv3 TLSv1 TLSv1.1 TLSv1.2;
        ssl_ciphers          HIGH:!aNULL:!MD5;
        server_name localhost;
        # full path to the project dir - the dir that contains the urls.py file
        root /path/to/project/dir/dummy;
        access_log /path/to/logdir/nginx_access.log;
        error_log /path/to/logdir/nginx_error.log;

        location /static/{
            autoindex on;
            # The path to the actual project directory here - the one which contains the static/ 
            # dir holding the static files for this project
            root /path/to/project/dir/dummy;
        }

        location / {
            proxy_pass_header Server;
            proxy_set_header Host $http_host;
            proxy_redirect off;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Scheme $scheme;
            proxy_connect_timeout 10;
            proxy_read_timeout 10;
            proxy_pass http://localhost:8000/;
        }

        error_page  404              /404.html;
        location = /404.html {
            root   /usr/share/nginx/html;
        }
  
        # redirect server error pages to the static page /50x.html
        #
        error_page   500 502 503 504  /50x.html;
        location = /50x.html {
            root   /usr/share/nginx/html;
        }
    }

The first ``server`` block is for HTTP(port 80). Since I want all HTTP requests to be redirected to use HTTPS, we return a HTTP 301(permanent redirect) to our HTTPS server (BTW, HTTPS standard port is 443).

The next ``server`` block configures SSL by pointing Nginx to the private key and certificate we created. Now in the settings module of your Django project, do these settings:

.. code-block:: python

    import os
    SESSION_COOKIE_SECURE = True
    STATIC_ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'static')

    # URL prefix for static files.
    # Example: "http://media.lawrence.com/static/"
    STATIC_URL = '/static/'

Once that is done, start up Nginx as root(on Fedora):

.. code-block:: text

    # service nginx start

If that does not work, simply start nginx:

.. code-block:: text

    # nginx

Now, from the virtualenv, start our "program" using ``supervisorctl``:

.. code-block:: text

    (dummy)# supervisorctl start dummy

Point your browser to ``https://localhost/`` to see your app working. Note that since the digital certificate we created is self-signed, browsers will typically show a warning when using HTTPS. This is normal. On *real* production systems using HTTPS, you should get your certificate signed by a CA. It costs some money to get the CA issue a signed certifcate to you, but it is worth it.

That is all there is to deploying a Django app on Nginx + Gunicorn over HTTPS.
  
.. _`Google AppEngine`: https://developers.google.com/appengine/
.. _`OpenShift`: https://openshift.redhat.com/app/
.. _`Gunicorn`: http://gunicorn.org/
.. _`Nginx`: http://wiki.nginx.org/Main
.. _`Supervisor`: http://supervisord.org/
