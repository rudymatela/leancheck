LeanCheck FAQ
=============


What is LeanCheck?
------------------

[LeanCheck] is a (property-based) testing tool for Haskell.  It provides a
`check` function that takes a property, tests it by automatically generating
test values, then reports the results.

More details on the [LeanCheck tutorial].


What is property-based testing (PBT)?
-------------------------------------

In property-based testing, tests are defined as functions returning a boolean
value which should be true for all choices of argument values.  We call these
tests properties.  A property-based testing tool can then generate arguments
automatically to search for a failing test case.

Property-based testing is also known as:
* property testing;
* parameterized unit testing.

More details on the [LeanCheck tutorial].


What are the differences between QuickCheck and LeanCheck?
----------------------------------------------------------

[QuickCheck] and [LeanCheck] are both property-based testing tools for Haskell.
Some of the differences follow:

__Test case generation.__  In QuickCheck, test cases are generated randomly for
types that are instances of the [`Arbitrary`] typeclass.  In LeanCheck, test
cases are generated enumeratively for types that are instances of the
[`Listable`] typeclass.  The next section in this FAQ expands on this
difference.

__Purity.__  Differently from QuickCheck, LeanCheck generators are purely
functional.  To get a list of all booleans you simply do:

	> list :: [Bool]
	[False, True]

To get a (infinite) list of all integers, you simply do:

	> list :: [Int]
	[0, 1, 2, 3, 4, 5, 6, 7, ...]


What are the differences between enumerative and random testing?
----------------------------------------------------------------

__Counterexample size__ When using enumerative testing, reported
counterexamples are guaranteed to be the smallest or simplest as possible.
Most of the times, the smaller the counterexample, the easier it is to find the
cause of the fault.  When doing random testing, you may sometimes get a bigger
counterexample and you'll have to resort to [shrinking] which is enabled by
default on [`Arbitrary`] instances that define a [`shrink`] function.
Depending on your datatype, shrinking can be effective.

	> import Test.LeanCheck
	> check $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
	*** Failed! Falsifiable (after 4 tests):
	[] [0,0]

	> import Test.QuickCheck
	> quickCheck . noShrinking $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
	*** Failed! Falsifiable (after 3 tests):
	[2,-1]
	[0,2]

	> quickCheck $ \xs ys -> xs `union` ys == ys `union` (xs :: [Int])
	*** Failed! Falsifiable (after 3 tests):
	[]
	[2,2]

__Existential properties__ Enumerative testing tools allow for the definition
of existential properties:

	prop_lessThanExists :: Natural -> Natural -> Bool
	prop_lessThanExists x y = x <= y ==> exists 1000 $ \z -> x + z == y

Random testing does not allow it without the use of sometimes-not-so-obvious
process of [Skolemization].

__Ease of use__ In my humble opinion, writing generators for enumerative
testing tools is easier than on random testing tools.  Nevertheless, the
availability of automatic derivation makes this a minor issue.


[LeanCheck]:    https://hackage.haskell.org/package/leancheck
[QuickCheck]:   https://hackage.haskell.org/package/QuickCheck
[LeanCheck tutorial]: doc/tutorial.md
[`Listable`]:  https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`Arbitrary`]: https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#t:Arbitrary
[shrinking]:   https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#v:shrink
[`shrink`]:    https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#v:shrink
[Skolemization]: https://en.wikipedia.org/wiki/Skolem_normal_form
