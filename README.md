llcheck
=======

**This is a work in progress.  Come back after a while.**

**The API is very likely to change in the near future**

llcheck is a simple enumerative property-based testing library.  It works by
producing *listings* of test values, which are essentially (possibly infinite)
lists of finite lists of same-sized values.  It is somewhat similar to [Feat] in
that regard.

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

llcheck works on all types that are `Listable`.
Custom `Listable` instances are created similarly to SmallCheck:

	data MyType = MyConsA
	            | MyConsB Int
	            | MyConsC Int Char
	            | MyConsD String

	instance Listable MyType where
	    listing = cons0 MyConsA
	         \++/ cons1 MyConsB
	         \++/ cons2 MyConsC
	         \++/ cons1 MyConsD

The type of listing is simply an infinite list of lists.
Each list has values of increasing size.

	listing :: Listable a => [[a]]

For convenience, there is also the function `list`,
which returns an infinite list of values of the bound type:

	list :: Listable a => [a]

So, for example:

	take 5 (list :: [(Int,Int)]) -- > [(0,0),(0,1),(1,0),(0,-1),(1,1)]

The `list` function can be used to debug your custom instances.  For recursive
types, take care with the order of the constructors or your listing function
might become non-terminating: put non-recursive constructors first in your
listing definition.


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

	$ cabal sandbox add-source ../path/to/llcheck

To use the files directly in your project:

	$ cp -r Test ../path/to/your-project


This was tested on GHC 7.10.  This library does not use any fancy extensions:
if it does not work on previous GHC versions, probably only *minor* changes are
needed.


[Feat]: https://hackage.haskell.org/package/testing-feat
[SmallCheck]: https://hackage.haskell.org/package/smallcheck
