TO DO list for LeanCheck
========================

List of things to do for LeanCheck.


misc
----

* improve `mk/haskell.mk`: pass ALLHS and LIBHS instead of HSS
  By making that distinction, haskell.mk will be able to handle Haddock.
  It will also be clearer what each parameter means.
  Note that ALLHS and LIBHS are not (but could be) the final names.


documentation
-------------

* add eg folder with some examples of testing using LeanCheck;

* on tutorial.md, write about how to create test programs;

* on data-invariant.md, write missing section;


v0.6.0
------

* implement stub `Test.LeanCheck.Function.*` modules;
