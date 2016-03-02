LeanCheck
=========

**This is a work in progress.  Come back after a while.**

**The API is very likely to change in the near future**

LeanCheck is a simple enumerative property-based testing library.  It works by
producing *tiers* of test values, which are essentially (possibly infinite)
lists of finite lists of same-and-increasingly-sized values.  It is similar to
[Feat] in that regard.

In this README, lines ending with `-- >` indicate expected return values.


Checking if properties are True
-------------------------------

To check if properties are True,
just use the function `holds :: Testable a => Int -> a -> Bool`.
It takes _two arguments_:
the _number of values_ to test
and a _property_ (function returning Bool),
then, it returns a boolean indicating whether the property holds.
See (ghci):

	import Test.Check
	import Data.List
	holds 100 $ (\xs -> sort (sort xs) == sort (xs::[Int]))  -- > True
	holds 100 $ (\xs -> [] `union` xs == (xs::[Int]))        -- > False


Finding counter examples
------------------------

To find counter examples to properties,
you can use the function `counterExample :: Testable a => Int -> a -> Maybe [String]`.
It takes _two arguments_:
the _number of values_ to test
and a _property_ (function returning Bool).
Then, it returns Nothing if no results are found or Just a list of Strings
representing the offending arguments to the property.
See (ghci):

	import Test.Check
	import Data.List
	counterExample 100 $ (\xs -> sort (sort xs) == sort (xs::[Int]))  -- > Nothing
	counterExample 100 $ (\xs -> [] `union` xs == (xs::[Int]))        -- > Just ["[0,0]"]
	counterExample 100 $ (\xs ys -> xs `union` ys == ys `union` (xs::[Int])) -- > Just ["[]","[0,0]"]


Current differences from SmallCheck/QuickCheck
----------------------------------------------

There is no `quickCheck/smallCheck/depthCheck` function that prints results.
You either have to use `holds` and `counterExample`,
which are not wrapped in an IO () thus only return the results without
printing.


Testing for custom types
------------------------

LeanCheck works on all types that are `Listable`.
Custom `Listable` instances are created similarly to SmallCheck:

	data MyType = MyConsA
	            | MyConsB Int
	            | MyConsC Int Char
	            | MyConsD String

	instance Listable MyType where
	  tiers = cons0 MyConsA
	       \/ cons1 MyConsB
	       \/ cons2 MyConsC
	       \/ cons1 MyConsD

The tiers function return a potentially infinite list of finite sub-lists (tiers).
Each tier has values of increasing size.

	tiers :: Listable a => [[a]]

For convenience, there is also the function `list`,
which returns an infinite list of values of the bound type:

	list :: Listable a => [a]

So, for example:

	take 5 (list :: [(Int,Int)]) -- > [(0,0),(0,1),(1,0),(0,-1),(1,1)]

The `list` function can be used to debug your custom instances.


More information / extra functions
----------------------------------

`Listable` class instances are more customizable than what is described here:
check source comments or haddock documentation for details.


Building / Installing
---------------------

To build:

	$ cabal build

To install:

	$ cabal install

To reference in a cabal sandbox:

	$ cabal sandbox add-source ../path/to/leancheck

To use the files directly in your project:

	$ cp -r Test ../path/to/your-project


This was tested on GHC 7.10, GHC 7.8, GHC 7.6 and GHC 7.4.
This library does not use any fancy extensions:
if it does not work on previous GHC versions,
probably only *minor* changes are needed.
It optionally depends on Template Haskell
(for automatic derivation of Listable instances).


[Feat]: https://hackage.haskell.org/package/testing-feat
[SmallCheck]: https://hackage.haskell.org/package/smallcheck
