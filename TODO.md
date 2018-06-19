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
