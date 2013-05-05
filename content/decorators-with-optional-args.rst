Decorators with optional arguments in Python
##############################################
:date: 2012-10-02
:title: Decorators with optional arguments
:author: Yati Sagade
:tags: python, decorators, programming
:category: python
:slug: decorators-optional-arguments
:summary: A powerful way of creating abstractions using decorators in Python.

Python decorators are one of the best features of the language, and I think 
that `this SO answer`_ describes them the best. I'll take an example similar 
to the example from that question itself. We want to write a decorator that, 
when used to decorate a function that returns a string, will wrap that string 
with the HTML bold tags.

Concretely, given a function `say_hello()`,

.. code-block:: python

    def say_hello(user):
        return 'hello, {}'.format(user)

We should be able to do this

.. code-block:: python

    @makebold
    def say_hello(user):
        return 'hello, {}'.format(user)

And call it to get this:

.. code-block:: python

    >>> say_hello('lama')
    '<b>hello, lama</b>'
    >>>

Doing that is simple. The makebold `decorator` can be written as

.. code-block:: python

    def makebold(func):
        def inner(*args, **kwargs):
            return '<b>{}</b>'.format(func(*args, **kwargs))
        return inner

Now what if we wanted to optionally specify a colour for the greeting?
Sure, we could write another decorator `colour` that takes as an argument the 
colour. But to build up on this contrived example, say I want to pass that
colour on to the makebold decorator when I want to specify a colour other than
black. Concretely, we should be able to do this

.. code-block:: python

    @makebold
    def say_hello(user):
        return 'hello, {}'.format(user)

    @makebold(colour='red')
    def say_bye(user):
        return 'See you, {}'.format(user)

And get output like this
    
.. code-block:: python

    >>> say_hello('Lama') 
    <b>hello, Lama</b>
    >>> say_bye('Lama')
    <span style="color: red;"><b>See you, Lama</b></span>
    >>> 

Now if we simply add an optional colour parameter to our `makebold` decorator,
it will look like this

.. code-block:: python

    def makebold(colour=None):
        def decorator(func):
            colour_open = ('<span style="color: {};">'.format(colour)
                           if colour else '')
            colour_close = '</span>' if colour else ''
            def inner(*args, **kwargs):
                return ('{colour_open}<b>{message}</b>{colour_close}'
                        .format(colour_open=colour_open,
                                colour_close=colour_close,
                                message=func(*args, **kwargs)))
            return inner
    return decorator

The thing to note is that we can no longer just return a function from the 
decorator that does the boldification. We must, instead, return a function that
is the actual decorator that takes the function.

But then, the decoration syntax changes to

.. code-block:: python

    @makebold()
    def say_hello(user):
        return 'hello, {}'.format(user)

    @makebold(colour='red')
    def say_bye(user):
        return 'See you, {}'.format(user)

Close enough, but definitely very ugly.

To achieve what we are after, we'll exploit the default function arguments in
Python. It turns out that the decorator we are after can be written like this

.. code-block:: python

    def makebold(func=None, colour=None):
        def decorator(func):
            colour_open = ('<span style="color: {};">'.format(colour)
                           if colour else '')
            colour_close = '</span>' if colour else ''
            def inner(*args, **kwargs):
                return ('{colour_open}<b>{message}</b>{colour_close}'
                        .format(colour_open=colour_open,
                                colour_close=colour_close,
                                message=func(*args, **kwargs)))
            return inner
        if func is not None:
            return decorator(func)
        return decorator

Now we can say this 

.. code-block:: python

    @makebold
    def say_hello(user):
        return 'hello, {}'.format(user)

    @makebold(colour='red')
    def say_bye(user):
        return 'See you, {}'.format(user)

And the calls will give us the expected output:

.. code-block:: python

    >>> say_hello('Lama') 
    <b>hello, Lama</b>
    >>> say_bye('Lama')
    <span style="color: red;"><b>See you, Lama</b></span>
    >>> 

Explanation
------------
In the first decoration, we just decorate using `@makebold` and not 
`@makebold()`. That passes our `say_hello()` function as the first parameter 
`func` to the `makebold` decorator, as we we meant this

.. code-block:: python

    say_hello = makebold(say_hello)

`say_hello` is passed as the first positional argument, which is, according to 
the signature of `makebold`, `func`. Now, on line 12 in the latest definition 
of `makebold`, we check if the `func` argument is `None`, which it isn't and 
hence, we pass `func` to the actual `decorator()` function and return whatever 
is returned - which we know, will be the function `inner()` defined inside `decorator()`. Hence, everytime this 
decorated `say_hello()` is called, it is actually a version of `inner()` being
called.

In the second case, we decorate by specifying the `colour` keyword argument, 
and very importantly, we make an *explicit call* to the decorator. In short, we
are asking for this:

.. code-block:: python

    say_bye = makebold(colour='red')(say_hello)

which is conceptually equivalent to:

.. code-block:: python

    real_decorator = makebold(colour='red')
    say_bye = real_decorator(say_bye)

In the call to `makebold()`, we specify *just* the `colour` kwarg, which 
leaves the `func` kwarg None (the default value). Now, on line 12, the check
for non-`None`ness of `func` fails and hence, we return the inner decorator
function(which is the one that is assigned to `real_decorator` above), which
can in turn, take `say_bye()` as its parameter.

Caveats
--------
This method relies on the default argument mechanism of Python. But code like
this could break it:

.. code-block:: python

    @makebold(colour='blue', func='yada')
    def curse(user):
        return '$%$%#@$^#%^@ {}'.format(user)

This will raise a `TypeError` saying that an `str` is not callable, which 
happens as our decorator tries to call its `func` parameter. I personally don't
think this is a problem, as if we added a check to see if `func` was callable 
in `makebold()`, we'd also likely raise a similar `TypeError` if it was not.

Additionally, this method requires the decorating code to always use kwargs,
which, again, is fine with me, as decorators are most of the time, part of a 
library and calling them with explicit kwargs is, IMHO, better than using 
positional args. However, there obviously are ways to write decorators that
take optional positional args, similar to this:

.. code-block:: python

    @makebold
    def say_hello(user):
        return 'hello, {}'.format(user)

    @makebold('red')
    def say_bye(user):
        return 'See you, {}'.format(user)


Achieving this behaviour requires a few changes in the `makebold` decorator,
but frankly, I wouldn't bother. 

.. _`this SO answer`: http://stackoverflow.com/a/1594484 
