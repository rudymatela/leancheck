Changelog for LeanCheck
=======================


upcoming (January 2024)
-----------------------

* improve error messages
* some code linting, refactoring and cleanup
* improve testing of LeanCheck itself


v1.0.0 (August 2022)
--------------------

This release indicates that the `Test.LeanCheck` API is now stable.
This is the same as v0.9.12 with just the version number change,
the major bump here serves just to indicate stability.
If your project works with the 0.9 series, it will work with the 1.0 series.


v0.9.12 (August 2022)
---------------------

* replace enumeration of `Fractional` and `Floating` types
  by the Calkin-Wilf sequence:
	- add `listFloating` and `listFractional`;
	- deprecate `tiersFloating` and `tiersFractional`;
* improve the changelog with timestamps and history down to v0.5.0;
* normalize code formatting throughout: indentation, spacing and width;
* improve testing of LeanCheck itself;
* test on GHC 9.2 and 9.4 (works with no changes).


v0.9.10 (June 2021)
-------------------

* `Test.LeanCheck.Utils.Type`: derive Typeable instances on GHC 7.8.
  Behaviour on newer GHCs (>= 7.10) versions is unaffected
  as they automatically derive Typeable instances for all types.


v0.9.8 (June 2021)
------------------

* `Test.LeanCheck.Utils.Type`: ~Typeable instances on GHC 7.10.~
  ~Behaviour on newer GHCs (>= 8.0) versions is unaffected~
  ~as they automatically derive Typeable instances for all types.~
  _Update:_ these instances were already present on v0.9.6.
  LeanCheck v0.9.8 is essentially the same as v0.9.6.


v0.9.6 (May 2021)
-----------------

* no code changes in what is exported by `Test.LeanCheck`
* `Test.LeanCheck.Utils.Types`: export the `A`, `B`, `C`, `D`, `E` and `F` types
* slightly improve README
* improve Makefile and tests
* replace Travis by GitHub Actions as the CI system


v0.9.4 (April 2021)
-------------------

* no code changes in what is exported by `Test.LeanCheck`
* add `errorToLeft` and `anyErrorToLeft` on `Test.LeanCheck.Error`
* add `?==?` and `!==!` on `Test.LeanCheck.Error`
* add `Test.LeanCheck.Function.List`
* add `Test.LeanCheck.Function.Ord`
* reduce default argument enumeration to 12 on `Test.LeanCheck.Function.Eq`
* add FAQ
* improve Makefile and test scripts


v0.9.3 (March 2020)
-------------------

* improve Haddock documentation
* use consistent code format
* improve CI scripts and Makefile


v0.9.2 (March 2020)
-------------------

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


v0.9.1 (February 2019)
----------------------

* fix bug in `genericTiers` where using it bound to a recursive datatype could
  cause an infinite loop;
* minor improvements in documentation and tests.


v0.9.0 (January 2019)
---------------------

* logo for LeanCheck;
* `Listable` instances to most types in the Haskell 2010 Language Report:
	- `Word<n>`;
	- `Int<n>`;
	- `Complex`;
	- etc...;
* minor improvements in documentation and README.


v0.8.0 (November 2018)
----------------------

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


v0.7.7 (October 2018)
---------------------

* Add a `changelog.md` file with the contents of git tag annotations:
  `git tag -ln99`.


v0.7.6 (October 2018)
---------------------

* Add experimental `Test.LeanCheck.Generic` module with automatic derivation
  of Listable instances through `GHC.Generics`;
* Improve Haddock documentation.


v0.7.5 (September 2018)
-----------------------

* Fix tests on systems with case-insensitive filesystems, like:
    - Windows;
    - Mac OS;
* Fix tests on GHC 8.6.

This release fixes just the tests of LeanCheck itself.  The LeanCheck library
is otherwise unaffected.


v0.7.4 (September 2018)
-----------------------

* Add list of providers on README;
* Minor fix in haddock.


v0.7.3 (August 2018)
--------------------

* Fix bug: add missing Hugs backport file to source distribution
  (GHC users were not affected by this);
* Improve tests so I don't forget to include files in the source distribution
  (cabal sdist) again.


v0.7.2 (August 2018)
--------------------

* Significantly improve documentation;
* Slightly improve tests.


v0.7.1 (July 2018)
------------------

* LeanCheck now works on Hugs-200607 (only minor changes were needed);
* Implement functions that calculate statistics: `Test.LeanCheck.Stats`;
* More stuff on `Utils`: `rational`, `okNum`;
* Improve tests;
* Improve build scripts;
* Minor assorted fixes.


v0.7.0 (December 2017)
----------------------

* Improved cabal file;
* Cabal package now has all files checked in on git repo;
* Add functions to compute Listable statistics (and some stubs);
* Improve tests;
* Code improvements (refactoring).


v0.6.7 (September 2017)
-----------------------

The only change in relation to v0.6.6 is a fixed build on Travis (the reference
output files were outdated).  The code of the tool is otherwise unchanged.


v0.6.6 (September 2017)
-----------------------

* Improve showing of functional counter-examples.


v0.6.5 (August 2017)
--------------------

* Export ordering from 'Test.LeanCheck.TypeBinding';
* Improve documentation;
* Improve tests.


v0.6.4 (August 2017)
--------------------

* Add and export the `Xs` constructor in `Utils`;
* Add type binding operators for up to 12 arguments.


v0.6.3 (July 2017)
------------------

* `Test.LeanCheck`: Add the `delay` and `reset` functions;
* `Test.LeanCheck.Tiers`: `normalizeT` stripping up to 12 empty tiers;
* Some refactoring;
* Add lots of new tests for LeanCheck itself.


v0.6.2 (March 2017)
-------------------

* Improve documentation;
* Improve tests of LeanCheck itself (including putting LeanCheck on CI).


v0.6.1 (March 2017)
-------------------

* `Test.LeanCheck.Error`: export `fromError`;
* `Test.LeanCheck.Tiers`: export `mapMaybeT`, `maybeConsN` and `catMaybesT`;
* Some code refactoring and internal experiments with function enumeration.


v0.6.0 (October 2016)
---------------------

* Add `deriveListableCascading`;
* Add `Listable` instances for `Word` and `Ordering`;
* Improve tests of LeanCheck itself.


v0.5.0 (July 2016)
------------------

* Improve documentation;
* Rename several functions;
* Add several utilities in `Utils`.


Earlier versions
----------------

Please refer to the git commit history.
