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

__Memory consumption.__  LeanCheck is more memory intensive when compared to
QuickCheck.  With LeanCheck you may run out of memory when you're running tens
of millions of tests depending on your datatype.



What are the differences between enumerative and random testing?
----------------------------------------------------------------

__Counterexample size.__ When using enumerative testing, reported
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

__Existential properties.__ Enumerative testing tools allow for the definition
of existential properties:

	prop_lessThanExists :: Natural -> Natural -> Bool
	prop_lessThanExists x y = x <= y ==> exists 1000 $ \z -> x + z == y

Random testing does not allow it without the use of sometimes-not-so-obvious
process of [Skolemization].

__Coverage.__ Random testing always hits different test cases, so in the long
term you may get more test coverage.  With enumerative testing you only get
more coverage when you configure more tests.

__Ease of use.__ In my humble opinion, writing generators for enumerative
testing tools is easier than on random testing tools.  Nevertheless, the
availability of automatic derivation makes this a minor issue.


What are the differences between LeanCheck and Feat?
----------------------------------------------------

[LeanCheck] and [Feat] are both size-bounded enumerative testing tools.  There
are a few differences.  Some are detailed in the following paragraphs.

__Enumeration.__  Choices when defining enumeration were different on LeanCheck
and Feat.  For some types, like `Bool` and `[Bool]` the enumeration is
isomorphic (has the same order).  For other types, like `Int` and `[Int]`, the
enumeration is different.  In LeanCheck, [`tiers`] of values are as thin as
possible, this makes the enumeration less likely to "blow-up" when tupling.
Take for example, the enumeration for trios of integers,
`(Int,Int,Int)`:

	> import Test.LeanCheck
	> take 10 $ map length (tiers :: [[(Int,Int,Int)]])
	[1,3,6,10,15,21,28,36,45,55]

	> import Test.Feat
	> take 10 $ map fst ( values :: [(Integer, [(Int,Int,Int)])] )
	[8,24,72,200,528,1344,3328,8064,19200,45056]

As you nest, the difference increases:

	> import Test.LeanCheck
	> take 10 $ map length (tiers :: [[ [(Int,Int,Int)] ]])
	[1,1,4,13,41,129,406,1278,4023,12664]

	> import Test.Feat
	> take 10 $ map fst ( values :: [(Integer, [[(Int,Int,Int)]]  )] )
	[0,1,8,88,968,10632,116752,1282048,14078080,154590400]

__Interface.__  The API of these tools differ in several ways.  Two examples:

* LeanCheck supports ([`Testable`]) properties with multiple arguments
  out-of-the-box whereas Feat requires, at least by default, properties to be
  uncurried.
* LeanCheck is configured by the number of tests whereas Feat is configured by
  the size of tests.

__Memory consumption.__  LeanCheck is more memory intensive than Feat.

__Random testing.__  Feat is able to do random testing.  LeanCheck is not (at
least not with good performance).

__Implementation.__  The [implementation and internals of LeanCheck] are
simpler than those of Feat.

__Tool support.__  Tool support for LeanCheck and Feat is different.
[LeanCheck has bindings] for [Tasty], [test-framework] and [Hspec].  As of Jan
2019, [Feat only has bindings for test-framework], nevertheless adding support
for Tasty and Hspec would be simple.

LeanCheck facilitates the use of [Extrapolate] to generalize counterexamples.


[LeanCheck]:    https://hackage.haskell.org/package/leancheck
[QuickCheck]:   https://hackage.haskell.org/package/QuickCheck
[Feat]:         https://hackage.haskell.org/package/testing-feat
[Extrapolate]:  https://hackage.haskell.org/package/extrapolate
[LeanCheck tutorial]: doc/tutorial.md
[`Listable`]:  https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`Testable`]:  https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Testable
[`tiers`]:     https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:tiers
[`Arbitrary`]: https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#t:Arbitrary
[shrinking]:   https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#v:shrink
[`shrink`]:    https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#v:shrink
[Skolemization]: https://en.wikipedia.org/wiki/Skolem_normal_form
[Tasty]:          https://github.com/feuerbach/tasty#readme
[test-framework]: https://haskell.github.io/test-framework/
[Hspec]:          https://hspec.github.io/
[LeanCheck has bindings]: https://github.com/rudymatela/leancheck#providers-for-tasty-test-framework-and-hspec
[Feat only has bindings for test-framework]: https://hackage.haskell.org/package/test-framework-testing-feat
[implementation and internals of LeanCheck]: https://hackage.haskell.org/package/leancheck/docs/src/Test.LeanCheck.Core.html
