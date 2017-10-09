TO DO list for LeanCheck
========================

List of things to do for LeanCheck.


misc
----

* improve `mk/haskell.mk`: pass ALLHS and LIBHS instead of HSS
  By making that distinction, haskell.mk will be able to handle Haddock.
  It will also be clearer what each parameter means.
  Note that ALLHS and LIBHS are not (but could be) the final names.

* parameterize number of tests in test programs and add slow-test target


documentation
-------------

* on tutorial.md, write about how to create test programs;

* on data-invariant.md, write missing section;


other improvements
------------------

* Port `discardLaterT`, `discardT` and `nubT` from Speculate here.
  See `fitspec/eg/alga` for details.

* add `classify` function to measure distribution of data:
  something like:

    classifyBy :: (a -> b) -> [a] -> [(b,a)]
	countsBy :: (a -> b) -> [a] -> [(b,Int)]

* further improve showing of functions, indead of showing:

    \p q -> case (p,q) of
            (False,False) -> False
            (False,True)  -> False
            (True,False)  -> True
            (True,True)   -> False

  actually show just:

    \p q -> case (p,q) of
            (True,False)  -> True
			_             -> False

  Some thinking may have to be done for:

    \x y -> case (x,y) of
	        (0,0) -> 0
			(0,1) -> 0
			(1,1) -> 1
			(1,0) -> 1
			(0,2) -> 0
			(1,1) -> 1
			(2,0) -> 1
			...

  Where: `const (const 1) ->/ [(1,const 0)]`.


* idea for restructuring Function modules (all under `Test.LeanCheck.Function`):

	Show                  -- exports just Show (a -> b)
	Listable              -- exports just Listable (a -> b), based on LoP

	Listable.ListsOfPairs -- exports just Listable (a -> b), based on LoP
	Listable.CoListable   -- exports just Listable (a -> b), based on CoL

	CoListable            -- exports just the CoListable typeclass
	ListsOfPairs          -- exports just the LoP auxiliary functions
	ShowFunction          -- exports just the ShowFunction typeclass

  This structure seems somehow more clear to me.  It also allows, in the future, adding:

	module Test.LeanCheck.Function.Listable.Mixed where
	import Test.LeanCheck.Function.CoListable   as CoL
	import Test.LeanCheck.Function.ListsOfPairs as LoP
	instance Listable (a -> b) where
	  tiers = LoP.functions tiers tiers \/ CoL.cotiers tiers

  so that the user gets an enumeration of functions with repetitions, but using
  a mixed strategy for generation of values.


later
-----

* implement stub `Test.LeanCheck.Function.*` modules;

* somehow, improve the improve the enumeration of `Char`s:

   list = [ ['a'], ['b','c'], ['d','e','f'], ... ]
      ||| [ [' '], ['\n'] ]
      ||| [ ['0'], ['1'], ['2'], ...]
      ||| ...
     where
     ||| is something that interleaves tiers of different lists...
