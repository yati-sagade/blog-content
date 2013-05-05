:title: Dynamic variables in Common Lisp
:tags: programming, lisp
:author: Yati Sagade
:slug: common-lisp-dynamic-variables
:summary: Using dynamic variables in Common Lisp
:date: 2013-03-29

Many a time, global variables are needed to store stuff like standard I/O
stream handles. In lisp, global variables are nicer to manage than their
counterparts in other languages. These variables are called dynamic variables
in Lisp, and as a convention, their names start and end with a \*. There are two
ways to define dynamic variables, DEFVAR and DEFPARAMETER. DEFVAR assigns to
the variable only if it is undefined, while DEFPARAMETER works regardless. Both
DEFVAR and DEFPARAMETER take a variable name, an initial value and an optional
documentation string. One can leave out the initial value in a DEFVAR, which
results in an unbound dynamic variable.

.. code-block:: common-lisp

    (defvar *count* 0
      "Count of widgets made so far.")

    (defparameter *gap-tolerance* 0.001
      "Tolerance to be allowed in widget gaps.")

Rebinding dynamic variables
-----------------------------

Sometimes, we need to be able to change the value of a global variable just for
a part of our code. For example, the \*standard-output\* global variable is
bound to the standard output stream. One might want a part of the code calling
functions that write to \*standard-output\* to actually write to a file instead
of writing it to stdout. We can do this by assigning to \*standard-output\*
a handle to our file, executing our code and resetting \*standard-output\* to
stdout. But a neater a way is provided by Lisp. Whenever a form introduces a
variable having the same name as a dynamic variable, all code within the
binding form and all code called by this code see the new binding. So for
example, we can do this:

.. code-block:: common-lisp

    (format t "This will be printed to stdout.~%")
    (let ((*standard-output* *some-other-stream*))
      (format t "This will be printed to a file.~%"))
    (format t "This will again be printed to stdout.~%")

What is important to note is that while lexical variable bindings are seen only
by code textually within the binding form of the variables(and closures, of
course), all code CALLED from a binding form of a dynamic variable also sees the
new binding. The binding is restored as soon as the binding form is done
executing.

An example:

.. code-block:: common-lisp

    CL-USER> (defvar *x* 10)
    *X*
    CL-USER> (defun foo () (format t "x: ~d~%" *x*))
    STYLE-WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
    FOO
    CL-USER> (foo)
    x: 10
    NIL
    CL-USER> (let ((*x* 100)) (foo))
    x: 100
    NIL
    CL-USER> (foo)
    x: 10
    NIL

