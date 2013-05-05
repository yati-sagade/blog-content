:Title: Calculating C(n, r) efficiently
:Tags: python, math, algorithms
:Author: Yati Sagade
:Date: 27-10-2012
:Slug: efficient-c-n-r

Recently, I needed to calculate the number of ways in which r items can be
chosen from n identical items. This is a classical counting problem and the
number of ways in which r items can be chosen from n items is also known as the
combination, $C(n,r)$,  ${}^{n}C_{r}$ or $ n \\choose r $

The expression for calculating `C(n, r)` is


$$ {n \\choose r} = \\frac{n(n-1)(n-2)...(n-r+1)}{r(r-1)...1} $$


or, equivalently, using factorials,

$$ {n \\choose r} = \\frac{n!}{r!(n-r)!} $$

One implementation would be to compute the constituent factorials and the
perform the multiplication and division on them. But this is bad for 2 reasons

- It is slow.
- For even decent problems like C(100, 2), where the answer is 4950, the
  intermediate factorials are very large. 100! is approximately 9.33e57 and
  hence, languages that do not have numeric types to hold that kind of a
  number will overflow, wrecking a havoc - all for a result that fits well
  within 2 bytes!


A better implementation should use this recurrence:

$$ {n \\choose r} =  \\frac{n}{r} {{n-1} \\choose {r-1}} $$

with

$$ {n \\choose 1} = n $$
$$ {n \\choose 0} = 1 $$
$$ {0 \\choose r} = 0 $$

so, a recursive implementation in Python would look like

.. code-block:: python

    def comb(n, r):
        '''
        Find the number of ways r items can be chosen from a pool of n
        identical items, with n >= r

        '''
        if r > n or n == 0:
            return 0
        if r == 0:
            return 1
        return n * comb(n - 1, r - 1) / r

An iterative implementation is also not very difficult to arrive at:

.. code-block:: python

    def comb(n, r):
        '''
        Find the number of ways r items can be chosen from a pool of n
        identical items, with n >= r

        '''
        if r > n:
            return 0
        result = 1
        for i in xrange(r):
            result *= n
            result /= (i + 1)
            n -= 1
        return result

