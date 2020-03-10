LeanCheck
=========

[![LeanCheck's Build Status][build-status]][build-log]
[![LeanCheck on Hackage][hackage-version]][leancheck-on-hackage]
[![LeanCheck on Stackage LTS][stackage-lts-badge]][leancheck-on-stackage-lts]
[![LeanCheck on Stackage Nightly][stackage-nightly-badge]][leancheck-on-stackage-nightly]

![LeanCheck logo][leancheck-logo]

LeanCheck is a simple enumerative [property-based testing] library.  Properties
are defined as Haskell functions returning a boolean value which should be
`True` for all possible choices of argument values.    LeanCheck applies
enumerated argument values to these properties in search for a counterexample.
Properties can be viewed as parameterized unit tests.

LeanCheck works by producing *tiers* of test values: a possibly infinite list
of finite sublists of same-and-increasingly-sized values.  This enumeration is
similar to [Feat]'s.  However, the ranking and ordering of values are defined
differently.  The interface is also different.

Throughout this README lines that begin with the [symbol `>`] indicate a line
entered into an interactive interpreter (`ghci`).  The result of evaluating the
expression is then printed on the following line.


Installing
----------

To install the latest LeanCheck version from Hackage, just run:

	$ cabal update
	$ cabal install leancheck


Checking if properties are True
-------------------------------

To check if properties are True,
just use the function [`holds`] `:: Testable a => Int -> a -> Bool`.
It takes _two arguments_:
the _number of values_ to test
and a _property_ (function returning Bool),
then, it returns a boolean indicating whether the property holds.
See (ghci):

	> import Test.LeanCheck
	> import Data.List
	> holds 100 $ \xs -> sort (sort xs) == sort (xs::[Int])
	True
	> holds 100 $ \xs -> [] `union` xs == (xs::[Int])
	False

As a rule-of-thumb, you should run holds for 500, 1 000, or 10 000 tests.
With more than that you may run out-of-memory depending on the types being
tested.


Finding counter examples
------------------------

To find counter examples to properties,
you can use the function [`counterExample`] `:: Testable a => Int -> a -> Maybe [String]`.
It takes _two arguments_:
the _number of values_ to test
and a _property_ (function returning Bool).
Then, it returns Nothing if no results are found or Just a list of Strings
representing the offending arguments to the property.
See (ghci):

	> import Test.LeanCheck
	> import Data.List

	> counterExample 100 $ \xs -> sort (sort xs) == sort (xs::[Int])
	Nothing

	> counterExample 100 $ \xs -> [] `union` xs == (xs::[Int])
	Just ["[0,0]"]

	> counterExample 100 $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
	Just ["[]","[0,0]"]


Checking properties like in SmallCheck/QuickCheck
-------------------------------------------------

To "check" properties like in [SmallCheck] and [QuickCheck]
automatically printing results on standard output,
you can use the function [`check`] `:: Testable a => a -> IO ()`.

	> import Test.LeanCheck
	> import Data.List

	> check $ \xs -> sort (sort xs) == sort (xs::[Int])
	+++ OK, passed 200 tests.

	> check $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
	*** Failed! Falsifiable (after 4 tests):
	[] [0,0]

The function [`check`] tests for a maximum of 200 tests.
To check for a maximum of `n` tests, use [`checkFor`] `n`.
To get a boolean result wrapped in `IO`, use [`checkResult`] or [`checkResultFor`].
There is no "quiet" option, just use [`holds`] or [`counterExample`] in that case.


Testing user-defined types
--------------------------

LeanCheck works on properties with [`Listable`] argument types.
[`Listable`] instances are declared similarly to SmallCheck:

	data MyType = MyConsA
	            | MyConsB Int
	            | MyConsC Int Char
	            | MyConsD String

	instance Listable MyType where
	  tiers = cons0 MyConsA
	       \/ cons1 MyConsB
	       \/ cons2 MyConsC
	       \/ cons1 MyConsD

For types that do not have a constraning data invariant, instances can be
automatically derived with [Template Haskell] by using [`deriveListable`] like
so:

	deriveListable ''MyType

The [`tiers`] function return a potentially infinite list of finite sub-lists
(tiers).  Each successive tier has values of increasing size.

	tiers :: Listable a => [[a]]

For convenience, the function [`list`] returns a potentially infinite list
of values of the bound type:

	list :: Listable a => [a]

So, for example:

	> take 5 (list :: [(Int,Int)])
	[(0,0),(0,1),(1,0),(0,-1),(1,1)]

The `list` function can be used to debug your custom instances.

[`Listable`] class instances are more customizable than what is described here:
check source comments or haddock documentation for details.


Standard Listable Instances
---------------------------

LeanCheck comes out-of-the-box with [`Listable`] instances for all types in the
[Haskell 2010 Language Report] with [the intentional exception of a few types].
The [leancheck-instances] package aims to support types in the
[Haskell Platform] -- `$ cabal install leancheck-instances`.


Providers for Tasty, test-framework and Hspec
---------------------------------------------

The following providers allow including LeanCheck properties into
[Tasty], [test-framework] or [Hspec] test suites.

* [LeanCheck provider for Tasty]
  -- `$ cabal install tasty-leancheck` ;
* [LeanCheck provider for test-framework]
  -- `$ cabal install test-framework-leancheck` ;
* [LeanCheck provider for Hspec]
  -- `$ cabal install hspec-leancheck` .


Memory usage
------------

Due to the way it is implemented (using lists of lists), LeanCheck can be quite
memory intensive if we set the maximum number of tests of a property to
millions of values (YMMV).

For the default maximum number of tests (500) you should be safe on most cases.
If you use 1 000 or 10 000 as the maximum number of tests for a property you're
also generally safe.  More than that, it is in a hit or miss basis.

For more details, see [LeanCheck memory usage].


Further reading
---------------

For a detailed documentation of each function, see
[LeanCheck's Haddock documentation].

For an introduction to property-based testing
and a step-by-step guide to LeanCheck, see the
[tutorial on property-based testing with LeanCheck]
\(`doc/tutorial.md` in the source repository).

LeanCheck is subject to a chapter in a [PhD Thesis (2017)].

[LeanCheck's Haddock documentation]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[tutorial on property-based testing with LeanCheck]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md
[LeanCheck memory usage]: https://github.com/rudymatela/leancheck/blob/master/doc/memory-usage.md

[`Listable`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`holds`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:holds
[`counterExample`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:counterExample
[`check`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:check
[`checkFor`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkFor
[`checkResult`]:    https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkResult
[`checkResultFor`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkResultFor
[`tiers`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:tiers
[`list`]:           https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:list
[`deriveListable`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:deriveListable

[property-based testing]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md
[Feat]: https://hackage.haskell.org/package/testing-feat
[SmallCheck]: https://hackage.haskell.org/package/smallcheck
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[PhD Thesis (2017)]: https://matela.com.br/paper/rudy-phd-thesis-2017.pdf

[symbol `>`]: https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810780208
[Template Haskell]: https://wiki.haskell.org/Template_Haskell

[Tasty]:          https://github.com/feuerbach/tasty#readme
[test-framework]: https://haskell.github.io/test-framework/
[Hspec]:          https://hspec.github.io/
[LeanCheck provider for Tasty]:          https://hackage.haskell.org/package/tasty-leancheck
[LeanCheck provider for test-framework]: https://hackage.haskell.org/package/test-framework-leancheck
[LeanCheck provider for Hspec]:          https://hackage.haskell.org/package/hspec-leancheck
[leancheck-instances]:                   https://hackage.haskell.org/package/leancheck-instances
[the intentional exception of a few types]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck-Basic.html
[Haskell 2010 Language Report]:          https://www.haskell.org/onlinereport/haskell2010/
[Haskell Platform]:                      https://www.haskell.org/platform/

[leancheck-logo]: https://github.com/rudymatela/leancheck/raw/master/doc/leancheck.svg?sanitize=true

[build-status]: https://travis-ci.org/rudymatela/leancheck.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/leancheck
[hackage-version]: https://img.shields.io/hackage/v/leancheck.svg
[leancheck-on-hackage]: https://hackage.haskell.org/package/leancheck
[stackage-lts-badge]:            https://stackage.org/package/leancheck/badge/lts
[stackage-nightly-badge]:        https://stackage.org/package/leancheck/badge/nightly
[leancheck-on-stackage]:         https://stackage.org/package/leancheck
[leancheck-on-stackage-lts]:     https://stackage.org/lts/package/leancheck
[leancheck-on-stackage-nightly]: https://stackage.org/nightly/package/leancheck
