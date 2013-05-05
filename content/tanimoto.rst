The Tanimoto score
===================
:title: The Tanimoto Coefficient
:summary: Calculating the Tanimoto coefficient
:Author: Yati Sagade
:tags: machine-learning
:slug: tanimoto-coefficient
:date: 25-11-2012

There exist a number of metrics to measure similarity between two items, like
the `Euclidean distance metric`_, the `Pearson correlation coefficient`_ and 
the Tanimoto score, and its special case, the `Jaccard coefficient/index`_.

Whenever evaluating similarity between two entities, say, people or items, it is
convenient to encode each entity as an $N$-dimensional vector, with each dimension
representing some feature. For example, if we have $N$ customers and $M$ products in 
the catalog, we may, for finding similar items, encode each item as an 
$N$-dimensional vector with each dimension being the rating given to that item by
the $i$-th user, using the value $0$ for no rating at all. 

But sometimes, the values of the features are two valued, i.e., for any vector
$ \\mathbf{X_{i}} $ = $ (x_{i1}, x_{i2}, x_{i3}, ..., x_{iN}) $, where $ x_{ik} $ is either ``0`` or some positive weight 
``Wk`` assigned to the k-th entry . This weight does **not** depend on i and hence, is
the same for all vectors. The Tanimoto coefficient for such a pair of vectors 
$ \\mathbf{X_{m}} $ and $ \\mathbf{X_{n}} $ is given by

$$ \\begin{equation} \\label{bg:intmod} T(X_{m}, X_{n}) = \\frac {X_{mn}}{X_{mm} + X_{nn} - X_{mn}} \\end{equation} $$

where $ X_{ij} $ is $ \\mathbf{X_{i}}.\\mathbf{X_{j}} $ (the vector inner product).
Note that the Jaccard index is a special case of the Tanimoto score where $ W_{k} $ = $ 1 $
for all $ k $, i.e., the two vectors $ \\mathbf{X_{m}} $ and $ \\mathbf{X_{n}} $ are binary/bit vectors.

Implementation in Python
--------------------------
	
.. code-block:: python

    def tanimoto_score(vec1, vec2, weights=None):
        '''
        Return the Tanimoto score between vec1 and vec2. 
        Args:
        	vec1, vec2: The two vectors to find the Tanimoto coefficient for. MUST be
        				of the same length
        Kwargs:
        	weights: If given, this must be an iterable of the same length as vec1 and 
        			 vec2. If kth element of weights, weights[k] = wk, it means that
        			 vec1[k] and vec2[k] can take up values of either 0 or wk. If not
        			 given, or when None, a value of (1, 1, ... upto len(vec1) elements)
        			 is assumed(i.e., the Jaccard index of the binary vectors vec1 and
        			 vec2 is returned in this case).
        
        '''
        N = len(vec1)
        if weights is None:
            valid_ranges = [(0.0, 1.0) for i in vec1]
        else:
            valid_ranges = [(0.0, w) for w in weights]

        assert N == len(vec2) == len(valid_ranges)

        v1v2, v1v1, v2v2 = 0., 0., 0.
        for i in xrange(N):
            if vec1[i] not in valid_ranges[i] or vec2[i] not in valid_ranges[i]:
                raise ValueError('Encountered invalid values. Expected one of {}, ' 
                                 'got {} and {}'
                                 .format(valid_ranges[i], vec1[i], vec2[i]))
            v1v2 += vec1[i] * vec2[i]
            v1v1 += vec1[i] * vec1[i]
            v2v2 += vec2[i] * vec2[i]
        
        return v1v2 / (v1v1 + v2v2 - v1v2)

If the overhead of the validation is not needed(the common case, maybe), the 
implementation gets simpler:

.. code-block:: python

    def tanimoto_score(vec1, vec2):
        '''
        Return the Tanimoto score between vec1 and vec2. 
        Args:
            vec1, vec2: The two vectors to find the Tanimoto coefficient for. MUST be
                        of the same length
        
        No validation is performed except same length checks. It is assumed that
        the caller passes properly weighted data.

        '''
        N = len(vec1)
        
        assert N == len(vec2)
        
        v1v2, v1v1, v2v2 = 0., 0., 0.
        for i in xrange(N):
            v1v2 += vec1[i] * vec2[i]
            v1v1 += vec1[i] * vec1[i]
            v2v2 += vec2[i] * vec2[i]

        return v1v2 / (v1v1 + v2v2 - v1v2)

Both these implementations return a value between 0 and 1, with a higher value
indicating more similarity.

.. _`Euclidean distance metric`: http://en.wikipedia.org/wiki/Euclidean_distance
.. _`Pearson correlation coefficient`: http://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient
.. _`Jaccard coefficient/index`: http://en.wikipedia.org/wiki/Jaccard_index

