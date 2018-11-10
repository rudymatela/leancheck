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

* add examples on `Test.LeanCheck.Function`'s Haddock;

* add examples to all functions exported by `Test.LeanCheck`.


other improvements
------------------

* set internal type of `Natural` to be `Integer` instead of `Int`;

* make `Natural` and `Nat` not generate negatives;

* pass `--name` to Haddock depending on its version;

* consider changing `listIntegral` to work for types without negatives;

* implement `conditionStatsT` (see `classStatsT`)
