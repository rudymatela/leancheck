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

__Performance.__  Feat is better in terms of runtime and memory when
enumerating "late" values.  With Feat, you can enumerate the billionth (1 000
000 000 th) value in an enumeration instantly in under 255kb:

	$ cabal install testing-feat
	...
	$ ghci
	> import Test.Feat
	> :set +s
	> index 1000000000 :: [Bool]
	[True,True,False,True,True,True,False,False,True,True,False,True,False,True,True,False,False,True,False,True,False,False,False,False,False,False,False,False,True]
	(0.02 secs, 255,288 bytes)

You'll probably run out of memory when doing that with LeanCheck.  LeanCheck
works best when we stick to thousands of values.  With more than that
your-mileage-may-vary.

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


Are LeanCheck property tests biased towards the first argument?
---------------------------------------------------------------

No and yes.

LeanCheck is only biased towards the first argument on the _very last tier_
being tested.  For all other tiers it is fair.

LeanCheck tries to be fair between tiers, but not within them. The idea is that
values on the same tier have the same "priority" when tested, none is more
important than others. One way to make it so that tests are always fair could
be to use size, instead of the number of tests as a parameter to holds and
check. And that's exactly what [SmallCheck] and [Feat] do -- on them you choose
the number of tests by configuring the maximum depth/size.

On LeanCheck, we choose to use the number of tests for two reasons:

* it just seems simpler from the user's perspective;
* it allows for a more precise control of runtime if you have expensive tests;

The drawback is that LeanCheck may end up being "unfair" to the last tier being
tested, being biased towards the first property and constructor arguments.
However that only happens for the _very last_ tier of values.

One way to still keep it simpler but loosing fine grained control of runtime,
is to define holds as follows:

```
-- | Does a property __hold__ for at least the given number of test values?
--
-- > holds 1000 $ \xs -> length (sort xs) == length xs
--
-- The actual number of tests may be higher than the given number of test
-- values to account for fairness: if we start testing a tier of values, we do
-- so until the end.
holdsForAtLeast :: Testable a => Int -> a -> Bool
holdsForAtLeast n = and
                  . map snd
                  . concatMap snd
                  . takeWhile ((<= n) . fst)
                  . accumulate 0
                  . resultiers
  where
  accumulate n [] = []
  accumulate n (xs:xss) = let n' = n + length xs
                          in  (n',xs) : accumulate n' xss
```

That way, it will be fair to the last tier being tested -- which is where the
unfairness may rise.

(cf. [LeanCheck GitHub issue #14](https://github.com/rudymatela/leancheck/issues/14))


[LeanCheck]:    https://hackage.haskell.org/package/leancheck
[QuickCheck]:   https://hackage.haskell.org/package/QuickCheck
[Feat]:         https://hackage.haskell.org/package/testing-feat
[Extrapolate]:  https://hackage.haskell.org/package/extrapolate
[SmallCheck]:   https://hackage.haskell.org/package/smallcheck
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
