Installing a stack for scientific computing in a virtualenv
=============================================================
:Title: A Python stack for Scientific computing.
:Tags: python, math, algorithms, scipy
:Author: Yati Sagade
:Date: 08-12-2012
:Slug: scipy-stack 

I have recently started doing quite a bit of numerical and scientific computing
in Python. Since Python is a general purpose language, the numerical computing
abilities are provided to it by some excellent mathematical packages. This post
describes how I got my environment set up.

It is needed to have Python installed, which is not a problem on most Linux
distros nowadays anyway. The Windows installation is also pretty 
straightforward (check out the `official Python website`_).

I am running 64-bit Debian Sid, but the library names should be easily 
obtainable for other (non-Debian based) distros as well.

I assume a virtualenv installation(you *are* using virtualenvs, right?)

We need some libraries to get us started. They can be installed like so:

.. code-block:: text

    # aptitude install libamd2.2.0 libblas3gf libc6 libgcc1 libgfortran3 \
      liblapack3gf libumfpack5.4.0 libstdc++6 build-essential gfortran \
      libatlas-dev libatlas3-base python python-all-dev gcc g++ libblas-dev \
      liblapack-dev

Next, we install `NumPy`_


.. code-block:: text

    (scicomp)$ pip install numpy

After this, it is `SciPy`_
    
.. code-block:: text

    (scicomp)$ pip install scipy

That is going to take a while, by the way.

Now, we would like to install `matplotlib`_, which is a 2-D plotting library
for Python. Now Matplotlib is not very friendly with pip/easy_install on many
systems. So, we will install from source.

First, clone the matplotlib repo:

.. code-block:: text

    $ git clone git://github.com/matplotlib/matplotlib.git

Then run ``setup.py`` without any args to see if all is well
    
.. code-block:: text

    (scicomp)$ cd matplotlib
    (scicomp)$ python setup.py

This should show you a list of requirements and whether they were found on 
your system or not. The important section is the backends section. If none of
the supported backends is installed on your system (e.g., Tk, PyGTK, PyQT etc), 
you will want to install one. We will go for the simplest, Tk for the TkAgg
backend.

.. code-block:: text

    # aptitude install tcl-dev tk-dev python-tk
    
After this, we can actually install matplotlib. Switch to the cloned repo 
directory and run

.. code-block:: text

    (scicomp)$ python setup.py install

This should install matplotlib in your virtualenv. Now, to play with all the
goodies we have, we might optionally want to install `ipython`_, a superior
alternative to the standard Python shell.

.. code-block:: text

    (scicomp)$ pip install ipython

Now, fire up ipython and check if all went well

.. code-block:: text

    (scicomp)$ ipython
    Python 2.7.3 (default, Sep  9 2012, 17:41:34) 
    Type "copyright", "credits" or "license" for more information.

    IPython 0.13.1 -- An enhanced Interactive Python.
    ?         -> Introduction and overview of IPython's features.
    %quickref -> Quick reference.
    help      -> Python's own help system.
    object?   -> Details about 'object', use 'object??' for extra details.

    In [1]: from matplotlib import pyplot

    In [2]: pyplot.pie([1,2,3])
    Out[2]: 
    ([<matplotlib.patches.Wedge at 0x44a9b50>,
      <matplotlib.patches.Wedge at 0x44ad1d0>,
      <matplotlib.patches.Wedge at 0x44ad810>],
     [<matplotlib.text.Text at 0x44ad150>,
      <matplotlib.text.Text at 0x44ad790>,
      <matplotlib.text.Text at 0x44add90>])

    In [3]: pyplot.show()

This should bring up a pie chart. If it does not, there is some problem with 
the installation of some package. Additionally, to do R like statistical
analysis, you might also want to install the superb `pandas` library.

.. code-block:: text

    (scicomp)$ pip install pandas


.. _`official Python website`: http://www.python.org
.. _`NumPy`: http://numpy.org
.. _`SciPy`: http://scipy.org
.. _`matplotlib`: http://matplotlib.org
.. _`ipython`: http://ipython.org/
.. _`pandas`: http://pandas.pydata.org/
