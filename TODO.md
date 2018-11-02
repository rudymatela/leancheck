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

* Improve `show (&&)` to:

    \x y -> case (x,y) of
            (True,True) -> True
            _ -> False

* Try simplifying with most specific cases first (the ones that have less
  occurences of return values) to see if it is possible to get a smaller
  function.  If not, return whatever the normal algorithm produces.

* Improve showing of constant functions.
  Instead of showing `\x -> case x of _ -> 0`, show just `\x -> 0`
  Instead of showing `\x y -> case (x,y) of _ -> 0`, show just `\x y -> 0`

* Replace unused arguments by `_` when showing functions:
  Instead of showing `\x y -> case (x,y) of (_,0) -> 0; _ -> 1`,
  show               `\x y -> case (_,y) of (_,0) -> 0; _ -> 1`.
