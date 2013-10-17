---
layout: post
description: Reusable graph searches
---

Implementations for graph search algorithms usually follow a common pattern.
Either a loop or a recursive function is involved which traverses the nodes
in the graph one by one until a success condition is encountered. The condition
might be that a node with a particular property is found, usually called the
goal node, or that the set of discovered nodes itself has some property.

In order to implement reusable search algorithms it's important to realize
that usually in the search process we have two separate concerns:

1. How we explore the graph
2. What are we interested to find

It may happen that these two concerns cannot be completely separated for the
sake of performance, but in most cases they are.

Let's name the two concerns, so that we may refer to them more easily.

The first phase shall be called `explore`. It's purpose is to discover nodes
in the graph based on a certain policy. It involves starting from a seed
node and iteratively reaching out to adjacent nodes. The order in which
the adjacent nodes are considered is usually the distinctive mark of the
algorithm.

The second phase we shall call `inspect`. In this phase we are analyzing 
discovered nodes with the intention of finding the solution to our problem.
While `explore` may be quite generic, `inspect` is tightly bound to what
we're trying to accomplish and therefore is specific to the a particular
problem instance. However, this phase can be described by some general-purpose
building blocks as we shall see below.

An interesting observation is that the two phases are actually part of a
producer-consumer setup. The explore phase is the producer, and the items it
produces are the discovered nodes. The inspect phase consumes the nodes
until it finds what it is looking for, or there are no more nodes to discover.

To allow these two phases to communicate we need a data structure that passes
information from the one to the other.

We must the take care that the explore phase does not discover all the nodes
of the graph at once. It only discovers new nodes when the inspection needs
it to. This way we're doing only the minimum amount of work to discover
our solution.

The simplest data structure to achieve this is a lazy list, or stream as it
is sometimes called.

We'll use Haskell the exemplify what we're talking about.

First let's provide some representation for graphs to work with.


{% highlight haskell %}

data Graph a = G { initial :: a, expand :: a -> [a] }

{% endhighlight %}


We'll be viewing graphs as ways to represent a problem solving processes. Nodes
in the graph represent states in the solving process. Each graph will have
an `initial` node - representing the initial state of the problem. It will also
have an `expand` function which gives us all the accessible states from a
given current state.

Usually, in problems, transitioning from a state `A` to a state `B` involves a
cost and our `expand` function should reflect this in it's type:

{% highlight haskell %}

expand :: a -> [(a, Cost)]

{% endhighlight %}

However, we're ignore this for the sake of simplicity.

As we've said, these states reside inside the graph's nodes. The node data
structure looks something like this:

{% highlight haskell %}

data Node a = Node { state :: a, parent :: Maybe (Node a), distance :: Int }

{% endhighlight %}

Each node has a `state`, an optional `parent` (the node that lead us to it) and
a `distance` representing how many steps it took to arrive from the initial state
to this node.

No we can finally give a type for the explore phase:

{% highlight haskell %}

type Explore a = Graph a -> [Node a]

{% endhighlight %}

So an exploration algorithm is a function which given a graph will provide us with a
lazy stream of nodes resulted from exploring the graph.

Let's look at the inspect part:

{% highlight haskell %}

type Inspect a sol = [Node a] -> sol

{% endhighlight %}

Inspection consumes the stream of nodes and returns a solution. What's the specific type
of that solution is of little importance right now - it can be anything.

With this to pieces in place we can define a `solve` function which given a graph, an
exploration algorithm and an inspection method it will return a solution.

{% highlight haskell %}

solve :: Explore a -> Inspect a sol -> Graph a -> sol
solve explore inspect = inspect . explore

{% endhighlight %}

So `solve` is just function composition between exploring and inspecting.

Let's try to provide some actual implementations for explore and inspect.

We'll exemplify explore with the classical graph traversals: depth-first search (DFS)
and breadth-first search (BFS).

Let's start with DFS:

{% highlight haskell %}

dfs :: Ord a => Explore a
dfs (G initial expand) = dfsRec [initialNode initial] S.empty where
    dfsRec [] _ = []
    dfsRec (n:ns) visited
        | S.member (state n) visited = dfsRec ns visited
        | otherwise = dfsRec (expandNode n ++ ns) (S.insert (state n) visited)

    expandNode n@(Node a _ d) = [Node b (Just n) (d+1) | b <- expand a]

initialNode :: a -> Node a
initialNode a = Node a Nothing 0

{% endhighlight %}

We're using a list as a stack to keep track of the elements we're about to visit
next. We're also using a set of states, to make sure we're not visiting the same
state twice.

How does BFS look? Well, as it turns out, BFS is quite similar to DFS. What changes
is how we're adding newly discovered nodes in the data-structure that keeps track
of what we should visit next. In DFS we were adding the new elements at the beginning
of the list, so that they can be visited right away. In BFS, we'll be using a queue -
the new elements will be added at the back of the list, so that the older ones are
added first.

Because the're so similar we can just write a more generic function called `search` which
abstracts away the insertion order in an argument:

{% highlight haskell %}

search :: Ord a => ([Node a] -> [Node a] -> [Node a]) -> Explore a
search insertOrder (G initial expand) = traverse [initialNode initial] S.empty where
    traverse [] _ = []
    traverse (n:ns) visited
        | S.member (state n) visited = traverse ns visited
        | otherwise = traverse (insertOrder (expandNode n) ns) (S.insert (state n) visited)

    expandNode n@(Node a _ d) = [Node b (Just n) (d+1) | b <- expand a]

    initialNode :: a -> Node a
    initialNode a = Node a Nothing 0

dfs, bfs :: (Ord a) => Explore a
dfs = search newestFirst
bfs = search oldestFirst

newestFirst, oldestFirst :: [Node a] -> [Node a] -> [Node a]
newestFirst = (++)
oldestFirst = flip (++)

{% endhighlight %}

DFS and BFS can, thus, be written using the same algorithm template, but different data
structure.

We should mention that for BFS a data structure like [Data.Sequence](http://www.haskell.org/ghc/docs/7.6.3/html/libraries/containers-0.5.0.0/Data-Sequence.html)
would give better complexity. But for our purposes a list will do.

Let's apply our algorithms to a concrete graph. We will be using one represented using adjacency lists:





