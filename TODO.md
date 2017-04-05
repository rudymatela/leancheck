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

* add diff test for IO functions (diff w/ model output and exit status)

* (?) on leancheck.cabal, add upper bound for template-haskell package


documentation
-------------

* add eg folder with some examples of testing using LeanCheck;

* on tutorial.md, write about how to create test programs;

* on data-invariant.md, write missing section;


v0.6.3
------

* Remove repetition on ListsOfPairs' `tiers :: [[Bool -> Bool]]` enumeration.

* Remove trailing empty tiers on
  `tiers :: [[Bool->()]]` and `tiers :: [[Nat2->()]]`.
  (See the output of `./bench/tiers`.)
  Or if removal is not easy, at least state _why_ it happens somewhere.

* Add `names` function to the ShowFunction typeclass that lists templates of
  names for variables of the type.
  Sadly, there is no way to do this without introducing a typeclass restriction
  on function arguments.  Make a separate `Argument` typeclass to do that?

* add `classify` function to measure distribution of data:
  something like:

    classifyBy :: (a -> b) -> [a] -> [(b,a)]
	countsBy :: (a -> b) -> [a] -> [(b,Int)]

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

v0.6.4
------

* implement stub `Test.LeanCheck.Function.*` modules;
