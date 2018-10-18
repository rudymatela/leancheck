Changelog for LeanCheck
=======================


upcoming
--------

* Add a `changelog.md` file with the contents of git tag annotations:
  `git tag -ln99`.


v0.7.6
------

* Add experimental `Test.LeanCheck.Generic` module with automatic derivation
  of Listable instances through `GHC.Generics`;
* Improve Haddock documentation.


v0.7.5
------

* Fix tests on systems with case-insensitive filesystems, like:
    - Windows;
    - Mac OS;
* Fix tests on GHC 8.6.

This release fixes just the tests of LeanCheck itself.  The LeanCheck library
is otherwise unaffected.


v0.7.4
------

* Add list of providers on README;
* Minor fix in haddock.


v0.7.3
------

* Fix bug: add missing Hugs backport file to source distribution
  (GHC users were not affected by this);
* Improve tests so I don't forget to include files in the source distribution
  (cabal sdist) again.


v0.7.2
------

* Significantly improve documentation;
* Slightly improve tests.


v0.7.1
------

* LeanCheck now works on Hugs-200607 (only minor changes were needed);
* Implement functions that calculate statistics: `Test.LeanCheck.Stats`;
* More stuff on `Utils`: `rational`, `okNum`;
* Improve tests;
* Improve build scripts;
* Minor assorted fixes.


v0.7.0
------

* Improved cabal file;
* Cabal package now has all files checked in on git repo;
* Add functions to compute Listable statistics (and some stubs);
* Improve tests;
* Code improvements (refactoring).


v0.6.7
------

The only change in relation to v0.6.6 is a fixed build on Travis (the reference
output files were outdated).  The code of the tool is otherwise unchanged.


v0.6.6
------

* Improve showing of functional counter-examples.


v0.6.5
------

* Export ordering from 'Test.LeanCheck.TypeBinding';
* Improve documentation;
* Improve tests.


Earlier versions
----------------

Please refer to the git commit history.
