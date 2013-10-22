---
layout: post
description: Discussing alternative algorithms for finding the k<sup>th</sup>
    smallest element in a collection
---

I've recently stumbled upon the classical problem of finding the k<sup>th</sup>
smallest item in a collection. This is also known as the
[k-th order statistic](http://en.wikipedia.org/wiki/Order_statistic) problem, and
it can be very shortly stated as follows:

"Given a list of N items and an ordering for those items, find the k<sup>th</sup> element in
the ordered list"

Finding the minimum, for example, is a particularization of this problem for `k = 0`
and finding the maximum is the variant for `k = N - 1`.

The discussion below also applies to a more general problem - finding the smallest K elements
from a collection. The algorithms will work with only small changes.

Being a classical problem you can imagine it has several classical solutions.

The first one that comes to mind is to just sort the list of elements and then
retrieve the element at position k. This is easily achievable and gives us a
complexity of &Theta;(NlogN) and no extra memory if we're fine with modifying the initial
collection.

However, we can do better. The best known solution to this problem is the
[Quickselect](http://en.wikipedia.org/wiki/Quickselect) algorithm
which is derived from [Quicksort](http://en.wikipedia.org/wiki/Quicksort). Quickselect
has linear expected time complexity and constant memory usage. It suffers from
the same issues as Quicksort - mainly it can get to quadratic time if
we're really unlucky, but for most cases it suffices.

There is also an [algorithm with guaranteed linear complexity](http://people.csail.mit.edu/rivest/pubs/BFPRT73.pdf),
but it's not straightforward to implement and for typical inputs it can be slower
than Quickselect.

Interestingly enough, there is another solution to this problem that involves
keeping an additional collection of size k containing the smallest k elements
at each step in the scanning of our list. Let's call this additional collection
min-list.

For each element we encounter - if it's smaller than the largest item in the
min-list, we replace this larger item with the element. Otherwise, we just continue
with our iteration.

As you might imagine, it helps if the min-list is implemented as a container
which can quickly tells us what's the largest element in the list at all times.
A heap has this property.

Here's the Scala code for this algorithm:


{% highlight scala %}

def select[T](xs: Array[T], k: Int)(implicit ordering: Ordering[T]): T = {
  require(k >= 0 && k < xs.length, "k is not within the bounds of the array")
  val minSet = mutable.PriorityQueue()
  // initializing the min-set with the first k values
  for (i <- 0 to k) {
    minSet += xs(i)
  }
  for (i <- k + 1 until xs.length) {
    val x = xs(i)
    if (ordering.lt(x, minSet.head)) {
      minSet.dequeue()
      minSet += x
    }
  }
  minSet.head
}


{% endhighlight %}

As you can see, I used a mutable.PriorityQueye which is backed by a binary heap.

The time complexity for this implementation is &Theta;(NlogK). It also uses additional
&Theta;(K) memory. So it is asymptotically slower than quick-select in both aspects.
However, for small ks the intuition is that it can be a worthy competitor.

Let's put it to the test. The quick-select version I'm using is depicted below:

{% highlight scala %}

def select[T: Ordering](xs: Array[T], k: Int): T = {
  require(k >= 0 && k < xs.length)
  shuffle(xs)
  var lo = 0
  var hi = xs.length - 1
  while (hi > lo) {
    val i = partition(xs, lo, hi)
    if (i > k) {
      hi = i - 1
    } else if (i < k) {
      lo = i + 1
    } else return xs(i)
  }
  xs(lo)
}

def partition[T](xs: Array[T], lo: Int, hi: Int)(implicit ordering: Ordering[T]): Int = {
  var i = lo + 1
  var j = hi
  val v = xs(lo)
  var continue = true
  while (continue) {

    while (i < hi && ordering.lt(xs(i), v)) {
      i = i + 1
    }

    while (j > lo && ordering.lt(v, xs(j))) {
      j = j - 1
    }

    if (i < j) {
      exchange(xs, i, j)
    } else {
      continue = false
    }
  }
  exchange(xs, lo, j)
  j
}

def exchange[T](xs: Array[T], i: Int, j: Int) = {
  val aux = xs(i)
  xs(i) = xs(j)
  xs(j) = aux
}

{% endhighlight %}

If you're going to shout that this is not idiomatic Scala, please consider
that this is just a translation of the algorithm as it is found
[here](http://algs4.cs.princeton.edu/23quicksort/).

So which one is faster? Let's benchmark these for `N = 10.000.000` and various
values of K. For fun, lets also add the "median of 3" variant of quick-select where
we choose the pivot more intelligently and also a version of the min-list algorithm
where we use `java.util.PriorityQueue` instead of the Scala collection.

So here are the results:

![Run time comparison (Lower is better)](/assets/images/select-time-comparison.png)

Looking at the graph we see the following things:

* As we might expect, quick-select has a steady curve and does not depend on `K`
* min-list is faster then quick-select for values of `K` up to 5% of `N`.
* Not a lot of difference between using `s.c.m.PriorityQueue` and
  `j.u.PriorityQueue`. Up until we reach the million limit the Scala version
  appears to be slightly faster. The Java version catches up afterwards.
* Selecting the pivot in quick-select using the median of 3 method gives some
  small improvements

Concluding, if you encounter this problem it may be worth-while to try out the min-list
algorithm described above, especially if k is small - under 5% of the total number of items.
The memory overhead in this case isn't very big and the benefits may be considerable.
Using min-list also has some other benefits: you don't need to copy the array if you want
your elements to remain in the same order.

Also, if you're processing the elements as a stream which may not fit in-memory, using
quick-select isn't really an option, while min-list will work quite well.

Unfortunately, min-list cannot be used for the instance of the problem where `K = N / 2`, also
known as the median of the collection. The memory & time overhead is just impractical.




