:title: Lexical variables in Common Lisp
:tags: programming, lisp
:author: Yati Sagade
:slug: common-lisp-lexical-variables
:summary: Using variables in Common Lisp
:date: 2013-03-27

Common Lisp is a dynamically, but strongly typed language. The variables
carry the type information that can be fetched at runtime, and hence, the type
errors are detected dynamically. In this way, it is most similar to Python,
which, too, is a dynamically but strongly typed language.

All values in Common Lisp conceptually are references objects. Assigning a
new value to a variable only makes it refer to the new object. It does not
change the referrent. This is very much like how Python names and bindings work
and unlike C++ references, which themselves are always const. Of course, if a
variable holds a reference to a mutable object, it is always possible to modify
it via the reference, and the changes will be visible via all other references
to that object.

A binding is the runtime manifestation of a variable. For example, when a
function is called, the variables in the parameter list of the function
definition are bound by Lisp to the objects that are the actual arguments of
that particular call. A single variable can end up having many different
bindings during the run of a program - it can even have multiple bindings at the
same time, e.g., for a function parameter variable during a recursive function
calls.

The LET special operator
---------------------------

Apart from DEFUN, another way of introducing variables is the LET special
operator. The general form of a LET command is this:

.. code-block:: common-lisp

    (let (variable*)
      body-form*)

Here, each `variable` is a variable initialization form, which is either a list
containing a variable name and an initial value or just a variable name, which
gets an initial value of NIL.

.. code-block:: common-lisp

    (let ((x 10) (y 20) z)
      (list x y z))

    ;; (10 20 NIL)

That code binds the variables x, y and z to values 10, 20 and NIL, respectively.

When a LET form is encountered, all the initialization forms are evaluated first
and then new bindings are created for the variables to the initial values
before any of the body forms are executed. Within the body of LET, the variable
names refer to the newly created bindings. After the LET, the names refer to
whatever they were referring to before the LET. Everytime a LET is entered, the
variables are rebound. The return value of a LET form is the value of the last
expression evaluated in its body.

Scope
------

The scope of both LET variables and function parameters is delimited by the form
they were introduced in, which in turn is called the binding form. Nesting
binding forms that introduce variables with the same name causes the binding of
the innermost variable shadows all outer bindings.

.. code-block:: common-lisp

    CL-USER> (defun foo (x)
	       (format t "Parameter: ~a~%" x)
	       (let ((x 2))
	         (format t "Outer LET: ~a~%" x)
	         (let ((x 3))
	           (format t "Inner LET: ~a~%" x))
     	         (format t "Outer LET: ~a~%" x))
	       (format t "Parameter: ~a~%" x))
	     
    FOO
    CL-USER> (foo 4)
    Parameter: 4
    Outer LET: 2
    Inner LET: 3
    Outer LET: 2
    Parameter: 4
    NIL

In general, any construct that introduces a new variable name that is usable
only within that construct is a binding form. For example, the DOTIMES macro,
which is a basic counting loop, introduces a counter variable that is
incremented at every iteration, upto a certain count.

.. code-block:: common-lisp

    (dotimes (x 10) (format t "~a " x))
    ;; 0 1 2 3 4 5 6 7 8 9 


Another binding form, called LET\* allows one to use previously defined
variables in the variable list to initialize a variable. e.g., this is possible:

.. code-block:: common-lisp

    (let* ((x 10)
           (y (* x x)) ; note that x was used to initialize y.
      (list x y))


Lexical variables and closures
---------------------------------

Lexical variables in Lisp are like the lexically scoped local variables of
other languages like Python, C++. A lexical variable binding can be accessed by
all code that lies textually within the binding form of that variable. But there
is a twist, when nested functions come into the play.

.. code-block:: common-lisp

    (let ((count 0)) #'(lambda () (setf count (1+ count))))

Here, as we enter the LET, a binding for `count` gets created, and the value
returned from the LET is a function that accesses the count binding from its
enclosing scope. All fine so far, since the lambda itself lies textually within
the binding form of the count variable. But, when this lambda is returned to
the caller of the LET, and invoked from there, that code is not textually
within the LET. But as it turns out, this works perfectly fine. 

.. code-block:: common-lisp

    (defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
    (funcall *fn*) ;; 1
    (funcall *fn*) ;; 2
    (funcall *fn*) ;; 3
    ...

As we can see, if count is a lexical variable, any bindings created for it are
retained for as long as needed, in this case, for as long as someone holds a
reference to the returned lambda. The inner lambda is a full closure.

The key point to note is that it is the bindings that are captured in an inner
function, and not just the value, which means that not only can the inner
function access the value of the captured variable, it can also assign new
values to it and they will persist between calls. 

A single closure can close over multiple variable bindings by simply referring
to them. Similarly, multiple closures can also capture the same binding.

Common Lisp - Dynamic variables
===================================

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

All variables defined using DEFVAR and DEFPARAMETER are declared globally
special. This means that whenever a binding form tries to create a binding
for a variable that has been declared special, a dynamic binding is created
instead of a normal, lexical binding.

These examples I got from folks in #lisp on Freenode made the difference
between lexical bindings and dynamic bindings even clearer to me:

.. code-block:: common-lisp

    (defun foo (x)
      (let ((l (lambda () x))
	    (x 3))
        (funcall l)))

    (foo 1) ;; returns 1

    (let ((l lambda () *x*)
	  (*x* 3))
      (funcall l)) ;; returns 3

Constants
---------------

Constants can be defined using DEFCONSTANT, which has the same form as
DEFPARAMETER,

.. code-block:: common-lisp

    (defconstant name initial-value-form [documentation-string])

Constants declared in this way are always global. They can't be rebound in
any way. The convention while naming a constant is to have the name start and
end with +plus-signs+.


Assignment
------------

Since all symbols evaluate to the value of the variable they name, getting the
value of a variable is as easy as referring to it. The workhorse for assignment
in Common Lisp is the SETF macro, which by virtue of being a macro, can examine
its arguments, and hence be a generic assignment operator. The general form is

.. code-block:: common-lisp

    (setf place value)

e.g., 

.. code-block:: common-lisp

    (setf x 10)

assigns the value 10 to the variable x. Assigning a new value to a
binding has no effect on any other bindings of that variable or on the actual
value stored in the binding prior to the assignment. Thus calling `foo`:

.. code-block:: common-lisp

    (defun foo (x)
      (setf x 10))

will not have any effect on any value outside `foo`. Concretely, a form like:

.. code-block:: common-lisp

    (let ((y 20))
      (foo y)
      (print y))

prints 20, not 10.

Multiple assignments can be done with one SETF:

.. code-block:: common-lisp

    (setf x 1 y 2)

will assign 1 to x and 2 to y. Also, since setf(like assignment operators in
most other languages) returns the assigned value, assignments can be chained:

.. code-block:: common-lisp

    (setf x (setf y (random 10)))

will set both x and y to the same random number. Note that this is similar to
`x = y = random.randint(0, 10)` in Python.

SETF can be used to assign not only to variables, but to any place that can hold
a value. Examples include, an array element, a field slot of a user-defined
object, an entry in a hashtable, etc. This is intuitive, as the assignment
operator `=` works in exactly the same way in most other languages - it can be
used to assign to object members, array elements, hashtable entries, regular
variables, etc.

SETFing a place which is a part of a larger object has the same semantics as
regular SETF on a variable - it does not affect the value that was stored
previously in that place. This is just like how Python/Java do it. 

Other ways to modify places
-----------------------------

While SETF is a very general assignment tool that is the go to place for almost
all assignment needs, certain assignment patterns are common enough to justify
the inclusion of their own operators. e.g., the increment operation, which is
used to increment a value by 1, can be written using SETF as

.. code-block:: common-lisp

    (setf x (+ x 1))

But increments and decrements are very commonplace and having more concise
ways of performing them makes sense.

.. code-block:: common-lisp

    (incf x) ;; Equivalent to (setf x (+ x 1))
    
    (decf x) ;; Equivalent to (setf x (- x 1))

INCF and DECF belong to a class of macros called modifier macros. Modifier
macros are built on top of SETF to modify the value at a place. When the place
argument to a modifier macro is an expression that needs to be evaluated once
(which typically is the case when evaluation of the place form results in some
side-effects), modify macros do the right thing. Generally, they are guaranteed
to evaluate both their arguments and the subforms of the place form exactly
once. For example, let us consider the following INCF:

.. code-block:: common-lisp

    ;; Increment a random element in *array*
    (incf (aref *array* (random (length  *array))))

A naive translation to SETF might look like this:

.. code-block:: common-lisp

    (setf (aref *array* (random (length *array*)))
          (1+ (aref *array* (random (length *array*)))))

But that call to RANDOM is not deterministic, the second AREF might not give the
same element of the array as the first call. A more sane approach here would be
to use a temporary variable to hold the random number in a LET and then use
that in both the AREFs. INCF takes care of all this.

Two other useful modifier macros are ROTATEF and SHIFTF. ROTATEF rotates the
values between the places it takes as arguments.

.. code-block:: common-lisp

    (rotatef a b) ;; swap the values in a and b

.. code-block:: common-lisp

    CL-USER> (let ((a 0) (b 1) (c 2))
    	       (format t "~a~%" (list a b c))
    	       (rotatef a b c)
    	       (format t "~a~%" (list a b c)))
    (0 1 2)
    (1 2 0)
    NIL

SHIFTF shifts the values in the places in its argument list, till the second
last argument, one place to the left. The second last place itself is given 
the last argument's value.

.. code-block:: common-lisp

    CL-USER> (let ((a 0) (b 1) (c 2))
    	   (format t "~a~%" (list a b c))
    	   (shiftf a b c)
    	   (format t "~a~%" (list a b c)))
    (0 1 2)
    (1 2 2)
    NIL

So here, a gets b's value, b gets c's. But c being the last argument, it is
treated as a filler value for the second last argument, b, and is not itself
modified. The value of the first argument is the return value of SHIFTF.

All in all, variables in Lisp are pretty easy to grok, especially if one
understands how names/bindings work in Python(as was the case with me). It is
fascinating how one of the earliest programming languages just gets variables
right, complete with dynamic typing. 
