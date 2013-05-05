Learning Common Lisp - Functions 1
=========================================
:Date: 18-02-2013
:Summary: Defining basic functions in Common Lisp.
:Tags: programming, lisp
:Author: Yati Sagade
:Slug: understanding-lisp-functions


I have finally started to learn Lisp, and I've chosen Common Lisp as my Lisp.
I'm following the excellent *free* book `Practical Common Lisp`_ by Peter
Seibel. Lisp syntax is very simple, and compared to other languages I've learnt,
it is a welcome breeze of fresh air. 

A Lisp function is defined using the DEFUN macro, like so:

.. code-block:: common-lisp

    (defun name (parameter*)
      "Optional docstring, a la Python."
      body-form*)


Any symbol can be used as a function name, and the convention is to use
hyphenated-words instead of underscore_separated_words or CamelCase. 

The parameter list of a function defines the variables that will hold the
arguments passed to the function when it is called. A function taking no args
has an empty parameter list, written as ().

A string literal can be present after the parameter list, which, if present,
serves the purpose of documenting the function. This string is associated with
the function's name and can be later retrieved using the DOCUMENTATION function.

Finally, the body of a function consists of muliple forms, each of which is
evaluated in order. The result of evaluation of the last form is returned as the
return value of the function. One can also explicitly return from any point in
the function body using the RETURN-FROM special operator.

Optional parameters
--------------------
A function taking optional parameters can be defined by first specifying all
the required params and then placing the symbol &optional followed by the names
of the optional params.

.. code-block:: common-lisp

    (defun make-rect (width &optional height)
      (list width (if height height width))

    (make-rect 10) ;; -> (10 10)
    (make-rect 10 20) ;; -> (10 20)

When an argument is absent for an optional param, it is bound to NIL.
Default values can be provided with a parameter as arbitrary expressions.

.. code-block:: common-lisp

    (defun make-rect (width &optional (height width))
      (list width height))

It is possible to check if the argument for a parameter was passed or not by
placing another variable, following the default value expression, which is
set to T when an argument is passed, else to NIL.

.. code-block:: common-lisp

    (defun make-rect (width &optional (height width height-supplied-p))
      (list width height))

Rest params
-------------
Rest params allow us to write functions accepting a variable number of
arguments. A catch-all parameter can be specified after the &rest symbol, which
captures all the arguments remaining after binding with required parameters.

.. code-block:: common-lisp

    (defun sum-of-squares (&rest args)
      (apply '+ (loop while args
                      collecting (let ((x (pop args))) (* x x)))))


.. code-block:: common-lisp

    (sum-of-squares 1 2 3) ;; 14

Keyword params
-----------------
After any required, optional and rest params in that order, we can specify
keyword parameters, which are optional and can be passed in independent of
their position in the param list. To specify keyword params, we must include the
symbol &key and any number of keyword argument specifiers after that.

.. code-block:: common-lisp

    (defun greet (&key message name)
       (format t "~a, ~a~%" (if message message "Hello") (if name name "Lama")))


Now, GREET can be called like so:

.. code-block:: common-lisp

    (greet) ;; Hello, Lama
    (greet :name "Yati") ;; Hello, Yati
    (greet :name "Yati" :message "Good morning") ;; Good morning, Yati

We can use default values and supplied-p params with keyword params as well:

.. code-block:: common-lisp

    (defun greet (&key (message "Hello") (name "Lama" name-supplied-p))
      (format t "~a, ~a~%" message name)
      (if (not name-supplied-p) (format t "I still don't know your real name")))

This can now be called as

.. code-block:: common-lisp

    (greet)
    ;; Hello, Lama
    ;; I do not know your name
    (greet :name "Lama")
    ;; Hello, Lama
    
Note that in the second call, passing an explicit value for :name did not print
the second "I do not know your name" message, even if the passed value was the
same as the default value.

In case we want to use a concise name for a param in the function, while at the
same time providing a descriptive param name to the caller, we can replace the
name of a keyword param by another list that contains a keyword to be used while
calling the function and the name for the param:

.. code-block:: common-lisp

    (defun rgb (&key ((:red r) 0) ((:green g) 0) ((:blue b)))
      (list r g b))

can be called as

.. code-block:: common-lisp

    (rgb :red #xfe :blue #xac)
    ;; (254 0 172)

When mixing the various types of parameters, the order should always be 

- Required params
- &optional params
- &rest params
- &key params

Combining optional and keyword params is not such a good idea

.. code-block:: common-lisp

    (defun foo (x &optional y &key z) (list x y z))
    ;; (foo 1 2 :z 3)
    ;; (1 2 3)
    ;;
    ;; (foo 1)
    ;; (1 NIL NIL)
    ;;
    ;; (foo 1 :z 3)
    ;; ERROR

The reason why the last call resulted in an error is that even though we meant
to pass the :z kwarg, Lisp would interpret the :z as the argument for the param
``y``, after which a keyword param or nothing is expected. But that leaves just 3
which will not be parsed as a legal kwarg.

Mixing rest and keyword params, is, however safe. When both &rest and &key
params are present in a function's parameter list, the passed in arguments,
after filling in any required and optional parameters, are ALL collected into
the &rest variable, along with the keywords and the appropriate &key params are
also bound to appropriate values.

.. code-block:: common-lisp

    (defun foo (&rest rest &key a b c) (list rest a b c))
    ;; (foo :a 12 :b 10)
    ;; ((:A 12 :B 10) 12 10 NIL)

But when &rest is followed by a &key, all that can be collected into the rest
parameter variable is the plist passed in to the function for the keyword
parameters.

Returning
----------
A function's return value is what the last form in the function's body
evaluates to. To explicitly return control as a result of say, breaking out on
a condition from nested control structures, the RETURN-FROM special operator is
used. It is a general operator used to break out of any code block defined by
the BLOCK special operator. DEFUN wraps the functions code in a code block using
BLOCK and gives it the same name as the function. Hence, we can break out of a
function using RETURN-FROM by passing it two arguments, the unquoted name of
the function(unquoted because it is not eval'd) to return from and the value to
return.

.. code-block:: common-lisp

    (defun foo (n)
      (dotimes (i 10)
        (dotimes (j 10)
	  (when (> (* i j) n)
	    (return-from foo (list i j))))))


This finds the first pair of numbers, both less than 10, whose product is less
than the argument n.





.. _`Practical Common Lisp`: http://www.gigamonkeys.com/book/
