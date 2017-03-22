LeanCheck
=========

[![Build Status][build-status]][build-log]
[![Hackage version][hackage-version]][leancheck-on-hackage]



LeanCheck is a simple enumerative [property-based testing] library.  Properties
are defined as Haskell functions returning a boolean value which should be
`True` for all possible choices of argument values.    LeanCheck applies
enumerated argument values to these properties in search for a counterexample.
Properties can be viewed as parameterized unit tests.

LeanCheck works by producing *tiers* of test values: a possibly infinite list
of finite sublists of same-and-increasingly-sized values.  This enumeration is
similar to [Feat]'s.  However, the ranking and ordering of values are defined
differently.  The interface is also different.

In this README, lines ending with `-- >` indicate expected return values.


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

	import Test.LeanCheck
	import Data.List
	holds 100 $ \xs -> sort (sort xs) == sort (xs::[Int])  -- > True
	holds 100 $ \xs -> [] `union` xs == (xs::[Int])        -- > False


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

	import Test.LeanCheck
	import Data.List

	counterExample 100 $ \xs -> sort (sort xs) == sort (xs::[Int])
	-- > Nothing

	counterExample 100 $ \xs -> [] `union` xs == (xs::[Int])
	-- > Just ["[0,0]"]

	counterExample 100 $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
	-- > Just ["[]","[0,0]"]


Checking properties like in SmallCheck/QuickCheck
-------------------------------------------------

To "check" properties like in [SmallCheck] and [QuickCheck]
automatically printing results on standard output,
you can use the function [`check`] `:: Testable a => a -> IO ()`.

	import Test.LeanCheck
	import Data.List

	check $ \xs -> sort (sort xs) == sort (xs::[Int])
	-- > +++ OK, passed 200 tests.

	check $ \xs ys -> xs `union` ys == ys `union` (xs::[Int])
	-- > *** Failed! Falsifiable (after 4 tests):
	-- > [] [0,0]

The function [`check`] tests for a maximum of 200 tests.
To check for a maximum of `n` tests, use [`checkFor`] `n`.
To get a boolean result wrapped in `IO`, use [`checkResult`] or [`checkResultFor`].
There is no "quiet" option, just use [`holds`] or [`counterExample`] in that case.


Testing user-defined types
--------------------------

LeanCheck works on properties with [`Listable`] argument types.
`Listable` instances are declared similarly to SmallCheck:

	data MyType = MyConsA
	            | MyConsB Int
	            | MyConsC Int Char
	            | MyConsD String

	instance Listable MyType where
	  tiers = cons0 MyConsA
	       \/ cons1 MyConsB
	       \/ cons2 MyConsC
	       \/ cons1 MyConsD

The [`tiers`] function return a potentially infinite list of finite sub-lists
(tiers).  Each successive tier has values of increasing size.

	tiers :: Listable a => [[a]]

For convenience, the function [`list`] returns a potentially infinite list
of values of the bound type:

	list :: Listable a => [a]

So, for example:

	take 5 (list :: [(Int,Int)]) -- > [(0,0),(0,1),(1,0),(0,-1),(1,1)]

The `list` function can be used to debug your custom instances.

[`Listable`] class instances are more customizable than what is described here:
check source comments or haddock documentation for details.


Further reading
---------------

For a detailed documentation of each function, see
[LeanCheck's Haddock documentation].

For an introduction to property-based testing
and a step-by-step guide to LeanCheck, see the
[tutorial on property-based testing with LeanCheck]
\(`doc/tutorial.md` in the source repository).

[LeanCheck's Haddock documentation]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html
[tutorial on property-based testing with LeanCheck]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md

[`Listable`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#t:Listable
[`holds`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:holds
[`counterExample`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:counterExample
[`check`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:check
[`checkFor`]:       https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkFor
[`checkResult`]:    https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkResult
[`checkResultFor`]: https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:checkResultFor
[`tiers`]:          https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:tiers
[`list`]:           https://hackage.haskell.org/package/leancheck/docs/Test-LeanCheck.html#v:list

[property-based testing]: https://github.com/rudymatela/leancheck/blob/master/doc/tutorial.md
[Feat]: https://hackage.haskell.org/package/testing-feat
[SmallCheck]: https://hackage.haskell.org/package/smallcheck
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck

[build-status]: https://travis-ci.org/rudymatela/leancheck.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/leancheck
[hackage-version]: https://img.shields.io/hackage/v/leancheck.svg
[leancheck-on-hackage]: https://hackage.haskell.org/package/leancheck
