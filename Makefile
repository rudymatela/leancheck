# Makefile for LeanCheck
#
# Copyright:   (c) 2015-2017 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = tests/test           \
        tests/test-derive    \
        tests/test-error     \
        tests/test-fun       \
        tests/test-funshow   \
        tests/test-io        \
        tests/test-operators \
        tests/test-tiers     \
        tests/test-types
EGS = \
	eg/test-bool \
	eg/test-sort
BENCHS = \
	bench/tiers-colistable \
	bench/tiers-listsofpairs \
	bench/tiers
LISTHS   = find src mk -name \*.hs
LISTOBJS = $(LISTHS) | sed -e 's/.hs$$/.o/'
ALLHS    = $(shell $(LISTHS))
ALLOBJS  = $(shell $(LISTOBJS))
OBJS = src/Test/LeanCheck.o \
       src/Test/LeanCheck/Function.o \
       src/Test/LeanCheck/Error.o
GHCIMPORTDIRS = src
# -dynamic is needed only for src/Test/LeanCheck/Derive.hs and tests/test-derive.hs
GHCFLAGS = -dynamic -O2

all: $(OBJS)

all-all: $(ALLOBJS)

test: $(patsubst %,%.test,$(TESTS)) diff-test

diff-test: diff-test-tiers diff-test-funtiers $(patsubst %,%.diff-test,$(EGS))

update-diff-test: update-diff-test-tiers update-diff-test-funtiers $(patsubst %,%.update-diff-test,$(EGS))

%.test: %
	./$<

eg/%.diff-test: %
	./$< | diff -rud tests/model/eg/$< -

eg/%.update-diff-test: %
	./$< >           tests/model/eg/$<

clean: clean-hi-o clean-haddock
	rm -f bench/tiers-colistable.hs
	rm -f bench/tiers-listsofpairs.hs
	rm -f bench/tiers-funlistable
	rm -f bench/tiers-funlistable.hs
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

ghci: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

list-hs:
	$(LISTHS)

list-objs:
	$(LISTOBJS)

legacy-test: # needs ghc-7.10 .. ghc-7.4 installed as such
	make clean && make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.6 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make test GHC=ghc-7.4 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-7.10 test
	cabal clean && cabal-ghc-7.8 test
	cabal clean && cabal-ghc-7.6 test
	cabal clean && cabal-ghc-7.4 test
	cabal clean && cabal test

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

markdown:
	pandoc README.md -o README.html
	pandoc doc/tutorial.md -o doc/tutorial.html
	pandoc doc/data-invariant.md -o doc/data-invariant.html

haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif} README.html

upload-haddock:
	./mk/upload-haddock-to-hackage

doc/index.html: $(ALLHS)
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(ALLHS) --no-print-missing-docs --title=leancheck
	@echo 'NOTE: please ensure that there are *only* 7'
	@echo '      undocumented functions on Test.LeanCheck'
	@echo '      as "OPTIONS_HADDOCK prune" is active'
	@echo '      to hide cons6...cons12'

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk

diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"               | diff -rud tests/diff/tiers             -
	./bench/tiers "Int"              | diff -rud tests/diff/tiers-Int         -
	./bench/tiers "Nat"              | diff -rud tests/diff/tiers-Nat         -
	./bench/tiers "Integer"          | diff -rud tests/diff/tiers-Integer     -
	./bench/tiers "Bool"             | diff -rud tests/diff/tiers-Bool        -
	./bench/tiers "Char"             | diff -rud tests/diff/tiers-Char        -
	# list s
	./bench/tiers "[()]"             | diff -rud tests/diff/tiers-Us          -
	./bench/tiers "[Int]"          6 | diff -rud tests/diff/tiers-Ints        -
	./bench/tiers "[Nat]"          6 | diff -rud tests/diff/tiers-Nats        -
	./bench/tiers "[Bool]"         6 | diff -rud tests/diff/tiers-Bools       -
	./bench/tiers "String"         6 | diff -rud tests/diff/tiers-String      -
	# pairs
	./bench/tiers "(Int,Int)"        | diff -rud tests/diff/tiers-Int,Int     -
	./bench/tiers "(Nat,Nat)"        | diff -rud tests/diff/tiers-Nat,Nat     -
	./bench/tiers "(Int,Int,Int)"  6 | diff -rud tests/diff/tiers-Int,Int,Int -
	./bench/tiers "(Nat,Nat,Nat)"  6 | diff -rud tests/diff/tiers-Nat,Nat,Nat -
	# lists & pairs
	./bench/tiers "[((),())]"        | diff -rud tests/diff/tiers-U,Us        -
	./bench/tiers "([()],[()])"      | diff -rud tests/diff/tiers-Us,Us       -
	# special lists
	./bench/tiers "Set Bool"         | diff -rud tests/diff/tiers-SetBool     -
	./bench/tiers "Set ()"           | diff -rud tests/diff/tiers-SetU        -
	./bench/tiers "Set Nat"          | diff -rud tests/diff/tiers-SetNat      -
	./bench/tiers "Set Nat2"         | diff -rud tests/diff/tiers-SetNat2     -
	./bench/tiers "Set Nat3"         | diff -rud tests/diff/tiers-SetNat3     -
	./bench/tiers "Bag Bool"         | diff -rud tests/diff/tiers-BagBool     -
	./bench/tiers "Bag ()"           | diff -rud tests/diff/tiers-BagU        -
	./bench/tiers "Bag Nat"          | diff -rud tests/diff/tiers-BagNat      -
	./bench/tiers "Bag Nat2"         | diff -rud tests/diff/tiers-BagNat2     -
	./bench/tiers "Bag Nat3"         | diff -rud tests/diff/tiers-BagNat3     -
	./bench/tiers "NoDup Bool"       | diff -rud tests/diff/tiers-NoDupBool   -
	./bench/tiers "NoDup ()"         | diff -rud tests/diff/tiers-NoDupU      -
	./bench/tiers "NoDup Nat"        | diff -rud tests/diff/tiers-NoDupNat    -
	./bench/tiers "NoDup Nat2"       | diff -rud tests/diff/tiers-NoDupNat2   -
	./bench/tiers "NoDup Nat3"       | diff -rud tests/diff/tiers-NoDupNat3   -

update-diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"               > tests/diff/tiers
	./bench/tiers "Int"              > tests/diff/tiers-Int
	./bench/tiers "Nat"              > tests/diff/tiers-Nat
	./bench/tiers "Integer"          > tests/diff/tiers-Integer
	./bench/tiers "Bool"             > tests/diff/tiers-Bool
	./bench/tiers "Char"             > tests/diff/tiers-Char
	# lists
	./bench/tiers "[()]"             > tests/diff/tiers-Us
	./bench/tiers "[Int]"          6 > tests/diff/tiers-Ints
	./bench/tiers "[Nat]"          6 > tests/diff/tiers-Nats
	./bench/tiers "[Bool]"         6 > tests/diff/tiers-Bools
	./bench/tiers "String"         6 > tests/diff/tiers-String
	# pairs
	./bench/tiers "(Int,Int)"        > tests/diff/tiers-Int,Int
	./bench/tiers "(Nat,Nat)"        > tests/diff/tiers-Nat,Nat
	./bench/tiers "(Int,Int,Int)"  6 > tests/diff/tiers-Int,Int,Int
	./bench/tiers "(Nat,Nat,Nat)"  6 > tests/diff/tiers-Nat,Nat,Nat
	# lists & pairs
	./bench/tiers "[((),())]"        > tests/diff/tiers-U,Us
	./bench/tiers "([()],[()])"      > tests/diff/tiers-Us,Us
	# special lists
	./bench/tiers "Set Bool"         > tests/diff/tiers-SetBool
	./bench/tiers "Set ()"           > tests/diff/tiers-SetU
	./bench/tiers "Set Nat"          > tests/diff/tiers-SetNat
	./bench/tiers "Set Nat2"         > tests/diff/tiers-SetNat2
	./bench/tiers "Set Nat3"         > tests/diff/tiers-SetNat3
	./bench/tiers "Bag Bool"         > tests/diff/tiers-BagBool
	./bench/tiers "Bag ()"           > tests/diff/tiers-BagU
	./bench/tiers "Bag Nat"          > tests/diff/tiers-BagNat
	./bench/tiers "Bag Nat2"         > tests/diff/tiers-BagNat2
	./bench/tiers "Bag Nat3"         > tests/diff/tiers-BagNat3
	./bench/tiers "NoDup Bool"       > tests/diff/tiers-NoDupBool
	./bench/tiers "NoDup ()"         > tests/diff/tiers-NoDupU
	./bench/tiers "NoDup Nat"        > tests/diff/tiers-NoDupNat
	./bench/tiers "NoDup Nat2"       > tests/diff/tiers-NoDupNat2
	./bench/tiers "NoDup Nat3"       > tests/diff/tiers-NoDupNat3


TLF = "import\ Test.LeanCheck.Function"
bench/tiers-listsofpairs.hs: bench/tiers.hs
	sed -e "s/$(TLF)/$(TLF).ListsOfPairs\n$(TLF).Show/" $< > $@

TLF = "import\ Test.LeanCheck.Function"
bench/tiers-colistable.hs: bench/tiers.hs
	sed -e "s/$(TLF)/$(TLF).CoListable\n$(TLF).Show/" $< > $@

TLF = "import\ Test.LeanCheck.Function"
bench/tiers-funlistable.hs: bench/tiers.hs
	sed -e "s/$(TLF)/$(TLF).FunListable\n$(TLF).Show/" $< > $@

bench/tiers-colistable: bench/tiers-colistable.hs src/Test/LeanCheck/Function/CoListable.hs

bench/tiers-listsofpairs: bench/tiers-listsofpairs.hs src/Test/LeanCheck/Function/ListsOfPairs.hs

bench/tiers-funlistable: bench/tiers-funlistable.hs src/Test/LeanCheck/Function/FunListable.hs

diff-test-funtiers: bench/tiers-listsofpairs.diff-test \
                    bench/tiers-funlistable.diff-test \
                    bench/tiers-colistable.diff-test

update-diff-test-funtiers: bench/tiers-listsofpairs.update-diff-test \
                           bench/tiers-funlistable.update-diff-test \
                           bench/tiers-colistable.update-diff-test

bench/tiers-%.diff-test: bench/tiers-%
	# functions
	$< "()->()"           | diff -rud tests/diff/tiers-$*-U-U         -
	$< "Bool->Bool"       | diff -rud tests/diff/tiers-$*-Bool-Bool   -
	$< "Bool->()"         | diff -rud tests/diff/tiers-$*-Bool-U      -
	$< "()->Bool"         | diff -rud tests/diff/tiers-$*-U-Bool      -
	$< "Int->Int"       6 | diff -rud tests/diff/tiers-$*-Int-Int     -
	$< "Nat->Nat"       6 | diff -rud tests/diff/tiers-$*-Nat-Nat     -
	$< "()->Nat"        6 | diff -rud tests/diff/tiers-$*-U-Nat       -
	$< "Nat->()"        6 | diff -rud tests/diff/tiers-$*-Nat-U       -
	$< "Int->Int->Int"  4 | diff -rud tests/diff/tiers-$*-Int-Int-Int -
	$< "Nat->Nat->Nat"  4 | diff -rud tests/diff/tiers-$*-Nat-Nat-Nat -
	$< "(Nat,Nat)->Nat" 4 | diff -rud tests/diff/tiers-$*-Nat,Nat-Nat -
	$< "Maybe Bool->Bool" | diff -rud tests/diff/tiers-$*-MBool-Bool  -
	$< "Bool->Maybe Bool" | diff -rud tests/diff/tiers-$*-Bool-MBool  -
	$< "Maybe Bool->Maybe Bool" | diff -rud tests/diff/tiers-$*-MBool-MBool -
	# functions of lists
	$< "[Bool]->[Bool]" 4 | diff -rud tests/diff/tiers-$*-Bools-Bools -
	$< "[Nat]->[Nat]"   4 | diff -rud tests/diff/tiers-$*-Nats-Nats   -
	$< "[Int]->[Int]"   4 | diff -rud tests/diff/tiers-$*-Ints-Ints   -
	# more functions
	$< "Nat2->Nat2"       | diff -rud tests/diff/tiers-$*-Nat2-Nat2   -
	$< "Nat2->Nat3"       | diff -rud tests/diff/tiers-$*-Nat2-Nat3   -
	$< "Nat3->Nat2"       | diff -rud tests/diff/tiers-$*-Nat3-Nat2   -
	$< "Nat3->Nat3"       | diff -rud tests/diff/tiers-$*-Nat3-Nat3   -

bench/tiers-%.update-diff-test: bench/tiers-%
	# functions
	$< "()->()"           > tests/diff/tiers-$*-U-U
	$< "Bool->Bool"       > tests/diff/tiers-$*-Bool-Bool
	$< "Bool->()"         > tests/diff/tiers-$*-Bool-U
	$< "()->Bool"         > tests/diff/tiers-$*-U-Bool
	$< "Int->Int"       6 > tests/diff/tiers-$*-Int-Int
	$< "Nat->Nat"       6 > tests/diff/tiers-$*-Nat-Nat
	$< "Nat->()"        6 > tests/diff/tiers-$*-Nat-U
	$< "()->Nat"        6 > tests/diff/tiers-$*-U-Nat
	$< "Int->Int->Int"  4 > tests/diff/tiers-$*-Int-Int-Int
	$< "Nat->Nat->Nat"  4 > tests/diff/tiers-$*-Nat-Nat-Nat
	$< "(Nat,Nat)->Nat" 4 > tests/diff/tiers-$*-Nat,Nat-Nat
	$< "Maybe Bool->Bool" > tests/diff/tiers-$*-MBool-Bool
	$< "Bool->Maybe Bool" > tests/diff/tiers-$*-Bool-MBool
	$< "Maybe Bool->Maybe Bool" > tests/diff/tiers-$*-MBool-MBool
	# functions of lists
	$< "[()]->[()]"     4 > tests/diff/tiers-$*-Us-Us
	$< "[Bool]->[Bool]" 4 > tests/diff/tiers-$*-Bools-Bools
	$< "[Nat]->[Nat]"   4 > tests/diff/tiers-$*-Nats-Nats
	$< "[Int]->[Int]"   4 > tests/diff/tiers-$*-Ints-Ints
	# more functions
	$< "Nat2->Nat2"       > tests/diff/tiers-$*-Nat2-Nat2
	$< "Nat2->Nat3"       > tests/diff/tiers-$*-Nat2-Nat3
	$< "Nat3->Nat2"       > tests/diff/tiers-$*-Nat3-Nat2
	$< "Nat3->Nat3"       > tests/diff/tiers-$*-Nat3-Nat3
