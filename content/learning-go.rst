Learning Go
=============
:title: Learning Go
:summary: My tour of Go
:Author: Yati Sagade
:tags: Go, programming
:slug: learning-go
:date: 20-09-2012

I decided to start learning Go when I started seeing too many posts on HN about
it. `This post`_ particularly got me curious about the language and so I
installed Go, started `gotour`_ and am loving every bit of it now.

The customary Hello World program looks like this in Go:

.. code-block:: go

	package main

	import "fmt"

	func main() {
		fmt.Println("Hello, World!!")
	}

Functions are declared using the ``func`` keyword.

.. code-block:: go

	package main

	import (
		"fmt"
	)

	func add(x int, y int) int {
		return x + y
	}

	func main() {
		fmt.Println(add(1, 2))
	}

A function can, like in Python, return multiple results like so:

.. code-block:: go

	// A function that takes two strings and returns two strings
	func swap(x, y string) (string, string) {
		return y, x
	}

Go functions can have result parameters that can be used to return values.

.. code-block:: go

	func divide(dividend, divisor int) (quotient, remainder int) {
		quotient, remainder = dividend / divisor, dividend % divisor
		return
	}

So, the return statement by itself returns the current values of the result 
parameters.

The ``var`` keyword declares variables like so:

.. code-block:: go

	var y int
	var x int = 1
	// vars can take types implied by the assigned values
	var i, b, s = 1, true, "hello" 
	// so i is an int, b is a bool and s is a string

Within a function, we may use this shorthand:

.. code-block:: go

	func foo() {
		x := 1 // same as var x int = 1
	}

But all statements outside a function must start with a keyword and hence, this
shorthand is not valid in a global scope.

Constants are also declared like vars, but using the keyword const.


Looping
--------

There is only one looping construct in Go, the ``for`` loop.

There are various usages for this versatile loop in Go. The very basic one is:

.. code-block:: go

	for i := 0; i < 10; i++ {
		fmt.Println(i)
	}

So this is just like the for loops we are used to in other C like languages, 
but without the parens around the for expressions. Also, the counter i is in 
scope only within the for loop.

The pre and the post expressions can be left out:

.. code-block:: go

	for ; i < 10; {
		...
	}

In Go, this can be written as:

.. code-block:: go

	for i < 10 {
		...
	}

which is exactly how one would use a while loop in other languages that have
while.
For an infinite loop, even the condition expression can be left out:

.. code-block:: go

	for {
		fmt.Println("infinity is greater that infinity")
	}

Conditionals
-------------

We have ``if`` statements in Go
	
.. code-block:: go

	import (
		"fmt"
		"math"
	)

	func Sqrt(x float64) string {
		if x < 0 {
			return sqrt(-x) + "i"
		}
		return fmt.Sprint(math.Sqrt(x))
	}

The parens around the if condition are not even optional in Go. They are gone.
But the braces are required after the if statement.

Like ``for``, the ``if`` statement can include a short statement to execute before 
checking the condition.

.. code-block:: go

	func IsPerfectSquare(x int) bool {
		if root := int(math.Sqrt(x)); root * root == x {
			return true
		}
		return false
	}

This calculates the integral part of the square root of x into root and checks
if root squared gives back x. The var ``root`` lives in the scope of the if 
statement only, and is not accessible from outside it.
However, the variables declared in an ``if``'s short statement are all available
in all the ``else`` clauses for that ``if`` statement.

We also have ``switch`` in Go, which is similar to what we are used to, with some
differences.

.. code-block:: go

    func NaiveFactorial(x int) bool {
        var ret int
        switch x {
        case 0: fallthrough
        case 1: ret = 1
        default: ret = x * NaiveFactorial(x - 1)
        }
        return ret
    }

As we see, there is no ``break`` statement required in each ``case`` clause
unlike in C/C++/Java. Instead, a ``break`` is the default after the block of
statements under a ``case`` are done executing. To fall-through to the next
``case`` clause(which is the default in C++/Java), we need to use the keyword
``falthrough``. 

The switch statement in Go can be used to replace if-else ladders like so:

.. code-block:: go
    
    if (c1) {
        ...
    } else if (c2) {
    
    }
    ...
    else {
        ...   
    }

where c1, c2... are boolean expressions, can be written concisely as 

.. code-block:: go

    switch {
    case c1: ...
    case c2: ...
    ...
    default: ...
    }


structs
--------

A ``struct`` is a collection of fields. It is Go's answer to composite data types.
A struct representing a 2-dimensional vertex can be implemented as:

.. code-block:: go

	type Vertex struct {
		x int
		y int
	}

We can create a struct value in two ways, by using struct literals or by using the
``new`` function.

.. code-block:: go

	func main() {
		var v Vertex = Vertex{1, 2} // struct literal. v.x = 1 and v.y = 2
		v2 := Vertex{y: 100, x: 400}
		var w *Vertex = new(Vertex) 
	}

Note that ``new()`` returns a pointer to a newly allocated region of memory with 
zeroed out values for the fields.

Fields can be accessed using the dot operator

.. code-block:: go

	func main() {
		v := Vertex{1, 2}
		fmt.Println(v)
		v.x = 2 * v.y
		fmt.Println(v)
	}

will print

.. code-block:: go

	{1 2}
	{4 2}


pointers
----------

Go has pointers, but no pointer arithmetic. Struct fields can be referenced 
through a struct pointer. But the indirection is transparent.

.. code-block:: go

	func main() {
		p := Vertex{1, 2}
		q := &p
		q.x = 100
		fmt.Println(p)
		fmt.Println(q)
	}

will print

.. code-block:: go

	{100 2}
	&{100 2}

Equivalent C code, on the other hand, would look like:
	
.. code-block:: c

	struct vertex {
		int x;
		int y;
	}

	int main()
	{
		struct vertex p = {.x = 1, .y = 2};
		struct vertex *q = &p;
		q->x = 100;
		printf("{%d %d}", p.x, p.y);
		printf("%p: &{%d %d}", q, q->x, q->y);
		return 0;
	}

So, to access a field through a pointer in C/C++, we use the `->` operator.
But in Go, there is no need to do that, as the indirection is implicit.
(i.e., we can say q.x instead of ``(*q).x``, which is equivalent to q->x).

Pointers can also be declared by taking the address of a struct literal

.. code-block:: go

	// p is a *Vertex pointing to a Vertex instance with x, y = 1, 2
	p := &Vertex{1, 2}

The new function is used to allocate memory for a variable of a particular type.
It returns a pointer to the new instance, which is zeroed out. It's invocation
is of the form

.. code-block:: go
	
	var ptr *T := new(T)

where T is some type.

.. code-block:: go

	var ptr *Vertex = new(Vertex)

will allocate space for a new Vertex instance and will return a pointer to that 
instance, which now has x = y = 0.

maps
------

maps are the Go versions of Python dicts. The difference is that maps are 
statically typed. A type `map[T]S` means a map that has keys of type `T` and
values of type `S`.

maps are created using the `make()` allocation primitive in Go.

.. code-block:: go

	func main() {
		var m map[string]Vertex = make(map[string]Vertex)
		m["Origin"] = Vertex{0, 0}
		fmt.Println(m)
	}

will print

.. code-block:: go

	map[Origin: {0 0}]

maps can also be created using map literals

.. code-block:: go

	var m = map[string]Vertex{
		"Origin": Vertex{0, 0},
		"Limit": Vertex{0, 10},
	}
	
	func main() {
		fmt.Println(m)
	}

will print

.. code-block:: go

	map[Origin: {0 0} Limit: {0 10}]

### map operations ###
insertion/updation

.. code-block:: go
	
	m[key] = value

retrieval
	
.. code-block:: go

	elem = m[key]

deletion

.. code-block:: go
	
	delete(m[key])

membership check

.. code-block:: go

	elem, ok = m[key]

if `key` is there in `m`, `ok` will be `true`, else, it will be `false`. 
If `key` is not there in `m`, the value of elem will be zero value of the 
type of the values in that map.


Slices
--------
A slice is a sequence of elements of the same type. A slice is an abstraction
on top of Go arrays. A slice of type T has the type []T. Slices are created 
using the make function or using slice literals.

.. code-block:: go

	func main() {
		p := []int{1, 2, 3, 4, 5} // A slice of ints, initialized using slice
								  // literals
		// create a slice using make. The second arg to make is the length 
		// and the third one is the capacity
 		q := make([]int, 0, 5) 
	}

A slice has two measures of size, the length and the capacity. The length of a 
slice is the number of elements in it. The capacity is the maximum number of 
elements the slice can hold. 
A slice can be resliced to return slices. But unlike in Python, slices of a slice
refer to the same memory as the parent slice and copies are not created.

.. code-block:: go

	func main() {
		sl := make([]int , 0, 5)
		fmt.Println(len(sl)) // 0
		fmt.Println(cap(sl)) // 5
		sl = sl[:cap(sl)] // reslicing and assignment to grow the slice upto its 
						  // capacity.
		fmt.Println(len(sl)) // 5
		fmt.Println(cap(sl)) // 5
	}

Accessing slice elements is the same as in Python.

### Iteration ###
One can iterate over a slice/array using the familiar 
	
.. code-block:: go

	sl = []int{1, 2, 3, 4}
	for i := 0; i < len(sl); i++ {
		fmt.Println(sl[i])
	}

Or, more succinctly, this can be done in a more Python like way using `range`

.. code-block:: go

	sl = []int{1, 2, 3, 4} 
	for e := range sl {
		fmt.Println(e)
	}

Or if both the index and the element are needed during iteration, we can do

.. code-block:: go

	sl = []int{1, 2, 3, 4}
	for i, e := range sl {
		fmt.Println(i, ": ", e)
	}

Here, i holds the looop counter and e contains the value, sl[i].

functions
-----------
Functions are defined using the `func` keyword. And functions are first class
values. Functions in Go are also full closures.

A function PowOf that returns a function that calculates some integer raised to 
some other can be written as

.. code-block:: go

    func PowOf(seed int) func(int) int {
		inner := func(power int) {
			var ret int = 1
			for i := 0; i < power; i++ {
                ret *= seed;
			}
			return ret
		}
		return inner
    }



.. _`This post`: http://dave.cheney.net/2012/09/03/another-go-at-the-next-big-language
.. _`gotour`: http://tour.golang.org/#1
