Wrapping OpenCV functions with Boost Python
=============================================================
:Date: 2013-05-11
:Summary: Exposing C++ OpenCV code to Python using Boost.
:Tags: python, C++, OpenCV
:Author: Yati Sagade
:Slug: numpy-boost-python-opencv
:Category: programming

Both C++ and Python are excellent languages that complement each other in many
ways. I have been working on interesting Computer Vision and Document Analysis
problem and I have had the need of offloading some performance critical code to
C++ and expose it neatly to the other pieces, which in turn are in Python.

Here, I will take a simpler problem as a running example - matrix
multiplication. We want to write a super-efficient C++ function that multiplies
two matrices and expose that function to be callable from Python. The Python
code should be able to pass in two ``numpy.ndarray`` objects to be multiplied
and get back the result in a ``numpy.ndarray``.

We will call our module ``matrmul`` and the function ``mul``. Let us see how
the calls to this function should look like from Python:

.. code-block:: python
    
    import numpy
    import matrmul # our module


    a = numpy.array([[1., 2., 3.]])
    b = numpy.array([[1.],
                     [2.],
                     [3.]])
    print(matrmul.mul(a, b)) # should print [[14.]]


With the interface in place, we now begin by starting a Boost Python module
with the ``mul`` function. In `matrmul.cpp`_, there is code 

.. code-block:: c++
    
    #include <iostream>
    #include <opencv2/imgproc/imgproc.hpp>
    #include <boost/python.hpp>
    #include "conversion.h"

    namespace py = boost::python;

    PyObject*
    mul(PyObject *left, PyObject *right)
    {
        NDArrayConverter cvt;
        cv::Mat leftMat, rightMat;
        leftMat = cvt.toMat(left);
        rightMat = cvt.toMat(right);
        auto c1 = leftMat.cols, r2 = rightMat.rows;
        // Work only with 2-D matrices that can be legally multiplied.
        if (c1 != r2)
        {
            PyErr_SetString(PyExc_TypeError, 
                            "Incompatible sizes for matrix multiplication.");
            py::throw_error_already_set();
        }
        cv::Mat result = leftMat * rightMat;

        PyObject* ret = cvt.toNDArray(result);

        return ret;
    }

    static void init()
    {
        Py_Initialize();
        import_array();
    }

    BOOST_PYTHON_MODULE(matrmul)
    {
        init();
        py::def("mul", mul);
    }

As a quick tour of that code, the main multiplication ``mul`` takes two
parameters of the type ``PyObject*``. The ``PyObject*`` type represents the
Python ``object`` type in the C API. Since Python is dynamically typed, the
function has to figure out what is the actual type of the objects being passed.
First we use the ``NDArrayConverter`` class to convert the two arguments(which
we believe will NumPy arrays) to the OpenCV ``cv::Mat`` type. Then we check if
the two matrices are multiplication-compatible. If not, a ``TypeError`` is
thrown using the code

.. code-block:: c++

    PyErr_SetString(PyExc_TypeError, 
                    "Incompatible sizes for matrix multiplication.");
    py::throw_error_already_set();

``py`` refers to the namespace ``boost::python``, which provides many helpful,
idiomatic C++ wrappers around the C/Python API. Then we multiply the two
``Mat`` objects and convert the result back to an ndarray using the
``NDArrayConverter::toNDArray()`` function the definition of which we shall
see in a moment.

The part that exports this function ``mul`` as a function in Python is

.. code-block:: c++

    BOOST_PYTHON_MODULE(matrmul)
    {
        init();
        py::def("mul", mul);
    }
    
So we use a macro defined by the Boost Python library that declares a Python
module called ``matrmul``. Within that declaration, we call ``init()`` that
in turn initializes the Python runtime and the numpy C library. The latter is
absolutely necessary, as without it, any calls to the numpy C API will cause
a segmentation fault. Then, we use ``py::def()`` to define a module level
function called ``mul`` (we could give any other name here - this is the name
seen by Python code). The second argument to ``py:def`` is of course, our
function.

Now, we need to define the actual conversion functions for

- Numpy ndarray to ``cv::Mat`` conversion

- ``cv::Mat`` to Numpy ndarray conversion

The code for doing these conversions has been copied(and modified slightly)
from the OpenCV sources, and resides in `conversion.h`_ and `conversion.cpp`_.
The high level class exposing functions to do the conversions is called
``NDArrayConverter``

.. code-block:: c++

    class NDArrayConverter
    {
    private:
        void init();
    public:
        NDArrayConverter();
        cv::Mat toMat(const PyObject* o);
        PyObject* toNDArray(const cv::Mat& mat);
    };

So, ``NDArrayConverter::toMat()`` takes a numpy ndarray, again as
a ``PyObject*``, tests whether it is a valid numpy array(needed, as
a ``PyObject*`` can point to any Python object) and returns the equivalent
``cv::Mat``. 

``NDArrayConverter::toNDArray()`` does the reverse - takes a reference to
a ``cv::Mat`` and returns a ``PyObject*`` that represents the numpy array which
can be returned to the Python runtime via Boost. All this code, including a few
tests in Python is present `here`_. 

Boost Python can automatically convert native types (like ``str``
⇋ ``std::string``, Python ``long`` ⇋ C++ ``long``, ``int``, etc.), but we need
to do the conversion ourselves when we have stuff like ndarrays that Boost
Python does not know about. So the process of wrapping a function can be
summed up as:

- Write the C++ function to take normal native types if native types is all
  you expect to be passed to it from Python. Boost takes care of the
  plumbing and type checking. 

- If the function(in Python) takes anything other than native types, write
  the C++ function to take ``PyObject*`` - the generic Python ``object``. 

- If using ``PyObject*``, convert to whatever format is expected(or throw
  a ``TypeError`` if a malformed object was passed) and produce the result.

- If the result is a standard C++ type that can be handled by Boost, no
  conversion is needed. Otherwise, convert that result to a ``PyObject*``
  and return this.

- Use ``BOOST_PYTHON_MODULE`` to expose your function to Python.

After the code is ready, we need to compile the code to a shared object file.
The easiest way to do that would be to use GNU make. The Makefile should
compile the conversion code(``conversion.cpp``), the module wrapper
code(``matrmul.cpp``) and link them together. We also need to pass the Boost
and OpenCV headers while compiling and must link against the Boost, Python and
OpenCV libs. A working Makefile can be cloned `from here`_ and tweaked to
specific needs. After compiling, one can start the python shell from the same
directory as ``matrmul.so`` and test the multiplication routine:

.. code-block:: pycon

    In [1]: import numpy as np

    In [2]: import matrmul

    In [3]: a = np.array([[1., 2., 3.]])

    In [4]: b = a.reshape(3, 1)

    In [5]: b
    Out[5]: 
    array([[ 1.],
           [ 2.],
           [ 3.]])

    In [6]: a.dot(b)
    Out[6]: array([[ 14.]])

    In [7]: matrmul.mul(a, b)
    Out[7]: array([[ 14.]])

Using the Makefile provided, you can also type ``make test`` to run the tests
in ``test.py``, which verifies that our code is correct. 

Once again, all the code, including the Makefile and the Python tests can be
found `here`_.


.. _`Boost::Python`: http://www.boost.org/doc/libs/1_53_0/libs/python/doc/index.html
.. _`here`: https://github.com/yati-sagade/opencv-ndarray-conversion
.. _`matrmul.cpp`: https://github.com/yati-sagade/opencv-ndarray-conversion/blob/master/matrmul.cpp
.. _`conversion.h`: https://github.com/yati-sagade/opencv-ndarray-conversion/blob/master/conversion.h
.. _`conversion.cpp`: https://github.com/yati-sagade/opencv-ndarray-conversion/blob/master/conversion.cpp  
.. _`from here`: https://github.com/yati-sagade/opencv-ndarray-conversion/blob/master/Makefile  
