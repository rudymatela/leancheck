Changelog for LeanCheck
=======================

v0.9.6
------

* no code changes in what is exported by `Test.LeanCheck`
* `Test.LeanCheck.Utils.Types`: export the `A`, `B`, `C`, `D`, `E` and `F` types
* slightly improve README
* improve Makefile and tests
* replace Travis by GitHub Actions as the CI system

v0.9.4
------

* no code changes in what is exported by `Test.LeanCheck`
* add `errorToLeft` and `anyErrorToLeft` on `Test.LeanCheck.Error`
* add `?==?` and `!==!` on `Test.LeanCheck.Error`
* add `Test.LeanCheck.Function.List`
* add `Test.LeanCheck.Function.Ord`
* reduce default argument enumeration to 12 on `Test.LeanCheck.Function.Eq`
* add FAQ
* improve Makefile and test scripts

v0.9.3
------

* improve Haddock documentation
* use consistent code format
* improve CI scripts and Makefile

v0.9.2
------

* rename most functions on `Test.LeanCheck.Utils.Operators`;
  deprecated names are provided;
* improve documentation:
	- 100% haddock coverage;
	- LeanCheck memory usage thoroughly documented;
* implement stub function `conditionStatsT`;
* improve function display on `Test.LeanCheck.Function.*`;
* fix some compiler warnings (newer GHC);
* improve build scripts;
* improve tests;
* update tests scripts to support the new cabal (`test/sdist`).


v0.9.1
------

* fix bug in `genericTiers` where using it bound to a recursive datatype could
  cause an infinite loop;
* minor improvements in documentation and tests.


v0.9.0
------

* logo for LeanCheck;
* `Listable` instances to most types in the Haskell 2010 Language Report:
	- `Word<n>`;
	- `Int<n>`;
	- `Complex`;
	- etc...;
* minor improvements in documentation and README.


v0.8.0
------

* export `tiersFractional` from `Core` and main module;
* improve `Listable` instance for `Float`s and `Double`s;
* improve `Show` instance for functions;
* improve Haddock documentation;
* remove experimental function enumeration modules,
  in favour of the working `ListsOfPairs` enumeration;
* add special `String` and `Char` types to `Utils.Types`;
* fix bug in the `Natural` type of the `Utils.Types` modules;
* force non-negativity in `Natural` and `Nat` types from `Utils.Types`;
* rename some exported symbols in the `ShowFunction` module;
* improve tests of LeanCheck itself.


v0.7.7
------

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
