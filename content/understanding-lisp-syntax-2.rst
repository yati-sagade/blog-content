Learning Common Lisp - Functions 2
=========================================
:Date: 24-03-2013
:Summary: Functions as data in Common Lisp.
:Tags: programming, lisp
:Author: Yati Sagade
:Slug: understanding-lisp-functions-2

In Lisp, functions are just another kind of object. The DEFUN macro creates a
new function object and gives a name to it, which is what is used to call the
function. Nameless functions can be created using LAMBDA.

There is a special operator called FUNCTION that, given an unquoted name of a
function, returns the function object by that name.

Once we have a function object, it can be invoked using either of the functions
FUNCALL and APPLY. When the number of arguments that the function takes is
known, FUNCALL can be used. It takes a function object as its first argument
and all subsequent arguments are passed to the function itself. i.e., 

.. code-block:: common-lisp

    (foo 1 2 3) === (funcall #'foo 1 2 3)


For example, this function `plot` takes a function, the min and max X values
and the step and plots a simple ASCII plot of the function in the range
[min, max]

.. code-block:: common-lisp

    (defun plot (fn min max step)
      (loop for i from min to max do
            (loop repeat (fn i) (format t "*"))
	    (format t "~%")))

    (plot #'(lambda (x) (* x x)) -5 5 .5)

will print

.. code-block:: text

    *************************
    *********************
    ****************
    *************
    *********
    *******
    ****
    ***
    *
    *
    
    *
    *
    ***
    ****
    *******
    *********
    *************
    ****************
    *********************
    *************************

When the arguments to be passed to a function are available as a list, APPLY
can be used conveniently. Say plot-data is a list containing a function, the
min and the max values, and the step value. Then, plot can be invoked as:

.. code-block:: common-lisp

   (apply #'plot plot-data)

APPLY expects the function as its first argument and then, it expects a list,
which is to eventually become the argument list of the passed in function. It
also supports loose arguments. Say plot-data only contained min, max and step.
We can still call the PLOT function to plot the exponential function as follows:

.. code-block:: common-lisp

    (apply #'plot #'exp plot-data)

This, in effect, clubs the arguments after the first argument into a single
list which is then passed to the function object given by the first argument.

Anonymous functions
--------------------

When a function is used only at a single place, such as to pass a callback to
another function, defining a named function might be an overkill, and one can
use anonymous functions in such cases. These are created using LAMBDA
expressions. A lambda expression has the following form:

.. code-block:: common-lisp

    (lambda (parameters) body)

A lambda expression can also be thought of as a function name where the name
itself specifies what the function does. We can use lambda expressions anywhere
we would use function names.

.. code-block:: common-lisp

    (funcall #'(lambda (x y) (+ x y)) 2 3)
    ;; 5

OR

.. code-block:: common-lisp

    ((lambda (x y) (+ x y)) 2 3)
    ;; 5

