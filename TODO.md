TO DO list for LeanCheck
========================

List of things to do for LeanCheck.


misc
----

* parameterize number of tests in test programs and add slow-test target


documentation
-------------

* on tutorial.md, write about how to create test programs;

* on data-invariant.md, write missing section;


other improvements
------------------

* implement `conditionStatsT` (see `classStatsT`)


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

* Refactor T.L.F.ShowFunction module by adding SimplifiedBindings &
  ClarifiedBindings

* Replace unused arguments by `_` when showing functions:
  Instead of showing `\x y -> case (x,y) of (_,0) -> 0; _ -> 1`,
  show               `\x y -> case (_,y) of (_,0) -> 0; _ -> 1`.
