Introduction to property-based testing (with LeanCheck)
=======================================================

This document introduces property-based testing.  The reader only needs to be
familiar with Haskell.  No previous knowledge of property-based testing is
assumed.  This document focuses on LeanCheck, but skills are transferable to
[other property-based testing tools](#other-property-based-testing-tools-for-haskell).

(If you are already familiar with property-based testing and just want to learn
how to use LeanCheck, you might be best served by reading
[LeanCheck's README file](../README.md).

The learning outcomes of each section are:

* [What is property-based testing?](#what-is-property-based-testing)
  --- what is property-based testing;

* [Example 1: testing `sort`](#example-1-testing-a-sort-implementation)
  --- how to use a property-based testing library (LeanCheck);

* [Example 2: testing `insert`](#example-2-testing-conditional-properties)
  --- how to test conditional properties

* [Example 3: testing `Stack`](#example-3-testing-user-defined-datatypes)
  --- how to apply property-based testing to functions over user-defined
  datatypes by declaring [`Listable`] typeclass instances;


What is property-based testing?
-------------------------------

In property-based testing, properties are defined as Haskell functions
returning a boolean value which should be `True` for all possible choices of
argument values.  These properties are applied enumerated or random argument
values in search for a counterexample.  This is perhaps better illustrated in
an example (see Example 1).


### Terminology

Property-based testing might be known with other names:

* property testing;
* [parameterized unit tests]: in the context of C# ([NUnit]) or Java ([JUnit]),
							  properties are viewed with unit tests with
							  arguments.


Example 1: testing a sort implementation
----------------------------------------

Lets imagine that we want to test an implementation of a (not-so-quick) `sort`
function:

    sort :: Ord a => [a] -> [a]
    sort []     = []
    sort (x:xs) = sort lesser ++ [x] ++ sort greater
      where
      lesser  = filter (< x) xs
      greater = filter (> x) xs

### In contrast --- unit testing

We can *unit test* the above implementation:

    testsPass  ::  Bool
    testsPass = and
      [ []       == sort ([]::[Int])
      , [1]      == sort [1]
      , [1,2,3]  == sort [1,2,3]
      , [1,2,3]  == sort [3,2,1]
      , [1..100] == sort [100,99..1]
      ]

If we evaluate `testsPass` on ghci, we will get `True` --- our implementation
of `sort` passes all our unit tests.

### Declaring properties

Alternatively, we use *property-based testing* to test the above
implementation.  We first declare a few properties:

    prop_elem :: Ord a => a -> [a] -> Bool
    prop_elem x xs =  elem x (sort xs) == elem x xs

    prop_ordered :: Ord a => [a] -> Bool
    prop_ordered xs =  ordered (sort xs)
      where
      ordered (x:y:xs) = x <= y && ordered (y:xs)
      ordered _        = True

    prop_length :: Ord a => [a] -> Bool
	prop_length xs =  length (sort xs) == length xs

Those properties are similar to unit tests, but are parameterized.  Instead of
defining the behavior of sort for specific values, each property defines the
behavior of `sort` for a range of values.

### Testing properties

By binding those properties to specific types and passing those properties as
arguments to the [`check`] function, we get:

    $ ghci
	> import Test.Check

    > check (prop_elem :: Int -> [Int] -> Bool)
    +++ OK, passed 200 tests.

    > check (prop_ordered :: [Int] -> Bool)
    +++ OK, passed 200 tests.

Internally, the function [`check`] enumerates arguments to those functions and
test whether properties hold.

### Finding and fixing bugs

But what happens when the function does not follow the properties?

    > check (prop_length :: [Int] -> Bool)
    *** Failed! Falsifiable (after 3 tests):
    [0,0]

The `check` function reports a failing counterexample.  We get a `False` value
when evaluating `prop_length [0,0]`.  We can investigate on GHCi:

	> prop_length [0,0]
	False
	> length (sort [0,0]) == length [0,0]
	False
	> length (sort [0,0])
	1
	> sort [0,0]
	[0]

If we look back at our definition of `sort`, we can see that we forgot to
account for repeated elements.  We should change `>` to `>=`:

	greater = filter (>= x) xs

After fixing that bug, `prop_length` will pass:

    > check (prop_length :: [Int] -> Bool)
    +++ OK, passed 200 tests.


Example 2: testing conditional properties
-----------------------------------------

The boolean operator [`==>`] can be used to construct conditional properties.

The function [`insert`] defined in [`Data.List`] inserts an element into a list
at the first position where it is less than or equal to the next element.
*If the list is already ordered, the resulting list will still be ordered:*

    prop_insertOrd x xs =  ordered xs ==> ordered (insert x xs)


Example 3: testing user-defined datatypes
-----------------------------------------

Consider the following implementation of a `Stack`:

    data Stack a = Stack a (Stack a)
                 | Empty
      deriving (Show,Eq)

    push :: a -> Stack a -> Stack a
    push x s = Stack x s

    pop :: Stack a -> (a, Stack a)
    pop (Stack x s) = (x,s)

We might want to test the following property:

    prop_popush :: a -> Stack a -> Bool
    prop_popush x s =  pop (push x s) == (x,s)

However, if we provide this property on ghci, we get an error:

    > check (prop_popush :: Int -> Stack Int -> Bool)
    <interactive>:x:1:
      No instance for (Listable (Stack Int))

Our `Stack` type should be made an instance of the [`Listable`] typeclass.
This way LeanCheck will have a way to know how to list values to be tested by
the property.  See [`Listable`] documentation for more.  In this case, the
instance is:

    instance Listable a => Listable (Stack a) where
      tiers = cons2 Stack
           \/ cons0 Empty

Now:

    > check (prop_popush :: Int -> Stack Int -> Bool)
    +++ OK, passed 200 tests.

LeanCheck also provides the function [`deriveListable`] to automatically derive
[`Listable`] instances for types that do not follow a data invariant (precondition).



Advantages of property-based testing
------------------------------------

Property-based testing has a few advantages over unit-testing:

* (+) scalability:
  after making a small change to a program, we might [`checkFor`] `50` tests;
  before making a major release, we may [`checkFor`] `1000` tests.
  A continuous integration system can be configured to run more test than what
  is usual on developers machines.
  
* (+) documentation:
  properties serve as a clear documentation of behaviour;

* (+) tool support:
  in Haskell there are several different property-based testing tools to choose
  from.


The disadvantage is:

* (-) Properties are comparatively harder to write than simple input and output
  test cases. (+) However, it might be easier to define good properties than a good
  selection of unit test cases.
  See "[Ranking programs using Black-Box testing (2010)]".

If you are unsure, you can always use *both* PBT and UT.


Other property-based testing tools for Haskell
----------------------------------------------

* [QuickCheck]      : randomized
* [SmallCheck]      : enumerative, depth-bounded
* [Lazy SmallCheck] : enumerative, depth-bounded, lazy, demand-driven
* [Feat]            : enumerative, size-bounded
* [LeanCheck]       : enumerative, size-bounded


Further reading
---------------

* [Using LeanCheck on functions over types with a data invariant](data-invariant.md)
* [Testing and tracing using QuickCheck and Hat](https://www.cs.kent.ac.uk/pubs/2003/1896/content.pdf)
* [QuickCheck's seminal paper (2000)](https://dl.acm.org/citation.cfm?id=1988046)
* [SmallCheck's paper (2008)](http://dl.acm.org/citation.cfm?id=1411292)

[`Listable`]: https://hackage.haskell.org/package/leancheck/docs/Test-Check.html#t:Listable
[`check`]: https://hackage.haskell.org/package/leancheck/docs/Test-Check-IO.html#v:check
[`checkFor`]: https://hackage.haskell.org/package/leancheck/docs/Test-Check-IO.html#v:checkFor
[`==>`]: https://hackage.haskell.org/package/leancheck/docs/Test-Check.html#v:-61--61--62-
[`deriveListable`]: https://hackage.haskell.org/package/leancheck/docs/Test-Check-Derive.html#v:deriveListable

[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[SmallCheck]: https://hackage.haskell.org/package/smallcheck
[Lazy SmallCheck]: https://hackage.haskell.org/package/lazysmallcheck
[Feat]: https://hackage.haskell.org/package/testing-feat
[LeanCheck]: https://hackage.haskell.org/package/leancheck

[parameterized unit tests]: http://research.microsoft.com/apps/pubs/default.aspx?id=77419
[NUnit]: http://www.nunit.org/index.php?p=parameterizedTests&r=2.5
[JUnit]: http://www.mkyong.com/unittest/junit-4-tutorial-6-parameterized-test/

[`insert`]: https://hackage.haskell.org/package/base/docs/Data-List.html#v:insert 
[`Data.List`]: https://hackage.haskell.org/package/base/docs/Data-List.html

[Ranking programs using Black-Box testing (2010)]: http://www.cse.chalmers.se/~nicsma/papers/ranking-programs.pdf

