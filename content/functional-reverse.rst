Reversing a list recursively
=============================
:Date: 21-04-2013
:Summary: A functional way of reversing a list.
:Tags: programming, scala, functional-programming
:Author: Yati Sagade
:Slug: functional-reverse

Suppose we have a list implementation in which every element (or node) in the
list contains a ``head``, which is the item at this node and a ``tail``, which is
rest of the list.

.. code-block:: scala

    trait List {
      def isEmpty: Boolean
      def head: Int
      def tail: List
    }

    class Cons(val head: Int, val tail: List) extends List {
      def isEmpty = false
    }

    object Nil extends List {
      def isEmpty = true
      def head = throw new NoSuchElementException("Nil.head")
      def tail = throw new NoSuchElementException("Nil.tail")
    }


One way to reverse this without any mutation is:

.. code-block:: scala
    
    def reversed(xs: List): List = {
      def aux(xs: List, acc: List) =
        if (xs.isEmpty) acc
        else reversed(xs.tail, new Cons(xs.head, acc))
      aux(xs, Nil)
    }

Here is how this works. Suppose we have a list [1, 2, 3, 4, 5]. This is
represented in our scheme as:

.. code-block:: text

                    []
                   /  \ 
                  1   []
                     /  \
                    2   []
                       /  \
                      3   []
                         /  \
                        4   []
                           /  \
                          5   Nil

Here, each node is a ``Cons`` and the left child of a node is the value at that
node and the right child is the tail, which can either be another ``Cons`` or
``Nil``.

The Scala code to create this list is:

.. code-block:: scala

    val l = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Nil)))))

Now, in the ``reversed`` function, all the work is done by the auxilliary inner
function ``aux``. If the argument ``xs`` to ``aux`` is empty(meaning a ``Nil``),
return the accumulator argument ``acc``. Otherwise, add the head of ``xs`` to
the accumulator and recursively call ``reversed`` with just the tail of ``xs``.
It is clear that the recursion always terminates, as ``xs.tail`` will always have
one element less than ``xs`` and eventually, a call to ``reversed(Nil, ...)`` will
be made, which just returns the second argument, breaking the recursion. Let
us trace the execution of reversed:

.. code-block:: text
    
    val l = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Nil)))))
    reversed(l)
    \_ aux([1, 2, 3, 4, 5], Nil)
       \_ aux([2, 3, 4, 5], [1])
          \_ aux([3, 4, 5], [2, 1])
             \_ aux([4, 5], [3, 2, 1])
                \_ aux([5], [4, 3, 2, 1])
                   \_ aux(Nil, [5, 4, 3, 2, 1])
                       |
    <------------------[5, 4, 3, 2, 1]



       
