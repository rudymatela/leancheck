# Makefile for LeanCheck
#
# Copyright:   (c) 2015-2018 Rudy Matela
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
        tests/test-stats     \
        tests/test-types
EGS = \
	eg/overflow \
	eg/higher-order \
	eg/test-bool \
	eg/test-list \
	eg/test-sort
BENCHS = \
	bench/tiers-colistable \
	bench/tiers-funlistable \
	bench/tiers-listsofpairs \
	bench/tiers-mixed \
	bench/tiers
LISTHS   = find src mk -name \*.hs
LISTOBJS = $(LISTHS) | sed -e 's/.hs$$/.o/'
ALLHS    = $(shell $(LISTHS))
ALLOBJS  = $(shell $(LISTOBJS))
OBJS = src/Test/LeanCheck.o \
       src/Test/LeanCheck/Function.o \
       src/Test/LeanCheck/Error.o
GHCIMPORTDIRS = src:tests
# -dynamic is needed only for src/Test/LeanCheck/Derive.hs and tests/test-derive.hs
GHCFLAGS = -dynamic -O2
HADDOCKFLAGS = --no-print-missing-docs

all: $(OBJS)

all-all: $(ALLOBJS)

test: $(patsubst %,%.test,$(TESTS)) diff-test

diff-test: diff-test-tiers diff-test-funtiers $(patsubst %,%.diff-test,$(EGS))

update-diff-test: update-diff-test-tiers update-diff-test-funtiers $(patsubst %,%.update-diff-test,$(EGS))

%.test: %
	./$<

eg/%.diff-test: eg/%
	./$< | diff -rud tests/diff/$<.out -

eg/%.update-diff-test: eg/%
	./$< >           tests/diff/$<.out

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

test-sdist:
	./tests/test-sdist

test-via-stack:
	stack test

legacy-test: # needs ghc-8.2 .. ghc-7.8 installed as such
	make clean  &&  make test GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

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
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"
	@echo "(on Arch Linux, use: cabal haddock --for-hackage --haddock-options=--optghc=-dynamic)"

doc/index.html: $(ALLHS)
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(ALLHS) $(HADDOCKFLAGS) --title=leancheck
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
	./bench/tiers "()"               | diff -rud tests/diff/tiers.out             -
	./bench/tiers "Int"              | diff -rud tests/diff/tiers-Int.out         -
	./bench/tiers "Nat"              | diff -rud tests/diff/tiers-Nat.out         -
	./bench/tiers "Integer"          | diff -rud tests/diff/tiers-Integer.out     -
	./bench/tiers "Bool"             | diff -rud tests/diff/tiers-Bool.out        -
	./bench/tiers "Char"             | diff -rud tests/diff/tiers-Char.out        -
	# list s
	./bench/tiers "[()]"             | diff -rud tests/diff/tiers-Us.out          -
	./bench/tiers "[Int]"          6 | diff -rud tests/diff/tiers-Ints.out        -
	./bench/tiers "[Nat]"          6 | diff -rud tests/diff/tiers-Nats.out        -
	./bench/tiers "[Bool]"         6 | diff -rud tests/diff/tiers-Bools.out       -
	./bench/tiers "String"         6 | diff -rud tests/diff/tiers-String.out      -
	# pairs
	./bench/tiers "(Int,Int)"        | diff -rud tests/diff/tiers-Int,Int.out     -
	./bench/tiers "(Nat,Nat)"        | diff -rud tests/diff/tiers-Nat,Nat.out     -
	./bench/tiers "(Int,Int,Int)"  6 | diff -rud tests/diff/tiers-Int,Int,Int.out -
	./bench/tiers "(Nat,Nat,Nat)"  6 | diff -rud tests/diff/tiers-Nat,Nat,Nat.out -
	# lists & pairs
	./bench/tiers "[((),())]"        | diff -rud tests/diff/tiers-U,Us.out        -
	./bench/tiers "([()],[()])"      | diff -rud tests/diff/tiers-Us,Us.out       -
	# special lists
	./bench/tiers "Set Bool"         | diff -rud tests/diff/tiers-SetBool.out     -
	./bench/tiers "Set ()"           | diff -rud tests/diff/tiers-SetU.out        -
	./bench/tiers "Set Nat"          | diff -rud tests/diff/tiers-SetNat.out      -
	./bench/tiers "Set Nat2"         | diff -rud tests/diff/tiers-SetNat2.out     -
	./bench/tiers "Set Nat3"         | diff -rud tests/diff/tiers-SetNat3.out     -
	./bench/tiers "Bag Bool"         | diff -rud tests/diff/tiers-BagBool.out     -
	./bench/tiers "Bag ()"           | diff -rud tests/diff/tiers-BagU.out        -
	./bench/tiers "Bag Nat"          | diff -rud tests/diff/tiers-BagNat.out      -
	./bench/tiers "Bag Nat2"         | diff -rud tests/diff/tiers-BagNat2.out     -
	./bench/tiers "Bag Nat3"         | diff -rud tests/diff/tiers-BagNat3.out     -
	./bench/tiers "NoDup Bool"       | diff -rud tests/diff/tiers-NoDupBool.out   -
	./bench/tiers "NoDup ()"         | diff -rud tests/diff/tiers-NoDupU.out      -
	./bench/tiers "NoDup Nat"        | diff -rud tests/diff/tiers-NoDupNat.out    -
	./bench/tiers "NoDup Nat2"       | diff -rud tests/diff/tiers-NoDupNat2.out   -
	./bench/tiers "NoDup Nat3"       | diff -rud tests/diff/tiers-NoDupNat3.out   -
	./bench/tiers "Map Bool Bool"    | diff -rud tests/diff/tiers-MapBoolBool.out -
	./bench/tiers "Map () ()"        | diff -rud tests/diff/tiers-MapUU.out       -
	./bench/tiers "Map Nat Nat"      | diff -rud tests/diff/tiers-MapNatNat.out   -
	./bench/tiers "Map Nat2 Nat2"    | diff -rud tests/diff/tiers-MapNat2Nat2.out -
	./bench/tiers "Map Nat3 Nat3"    | diff -rud tests/diff/tiers-MapNat3Nat3.out -

update-diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"               > tests/diff/tiers.out
	./bench/tiers "Int"              > tests/diff/tiers-Int.out
	./bench/tiers "Nat"              > tests/diff/tiers-Nat.out
	./bench/tiers "Integer"          > tests/diff/tiers-Integer.out
	./bench/tiers "Bool"             > tests/diff/tiers-Bool.out
	./bench/tiers "Char"             > tests/diff/tiers-Char.out
	# lists
	./bench/tiers "[()]"             > tests/diff/tiers-Us.out
	./bench/tiers "[Int]"          6 > tests/diff/tiers-Ints.out
	./bench/tiers "[Nat]"          6 > tests/diff/tiers-Nats.out
	./bench/tiers "[Bool]"         6 > tests/diff/tiers-Bools.out
	./bench/tiers "String"         6 > tests/diff/tiers-String.out
	# pairs
	./bench/tiers "(Int,Int)"        > tests/diff/tiers-Int,Int.out
	./bench/tiers "(Nat,Nat)"        > tests/diff/tiers-Nat,Nat.out
	./bench/tiers "(Int,Int,Int)"  6 > tests/diff/tiers-Int,Int,Int.out
	./bench/tiers "(Nat,Nat,Nat)"  6 > tests/diff/tiers-Nat,Nat,Nat.out
	# lists & pairs
	./bench/tiers "[((),())]"        > tests/diff/tiers-U,Us.out
	./bench/tiers "([()],[()])"      > tests/diff/tiers-Us,Us.out
	# special lists
	./bench/tiers "Set Bool"         > tests/diff/tiers-SetBool.out
	./bench/tiers "Set ()"           > tests/diff/tiers-SetU.out
	./bench/tiers "Set Nat"          > tests/diff/tiers-SetNat.out
	./bench/tiers "Set Nat2"         > tests/diff/tiers-SetNat2.out
	./bench/tiers "Set Nat3"         > tests/diff/tiers-SetNat3.out
	./bench/tiers "Bag Bool"         > tests/diff/tiers-BagBool.out
	./bench/tiers "Bag ()"           > tests/diff/tiers-BagU.out
	./bench/tiers "Bag Nat"          > tests/diff/tiers-BagNat.out
	./bench/tiers "Bag Nat2"         > tests/diff/tiers-BagNat2.out
	./bench/tiers "Bag Nat3"         > tests/diff/tiers-BagNat3.out
	./bench/tiers "NoDup Bool"       > tests/diff/tiers-NoDupBool.out
	./bench/tiers "NoDup ()"         > tests/diff/tiers-NoDupU.out
	./bench/tiers "NoDup Nat"        > tests/diff/tiers-NoDupNat.out
	./bench/tiers "NoDup Nat2"       > tests/diff/tiers-NoDupNat2.out
	./bench/tiers "NoDup Nat3"       > tests/diff/tiers-NoDupNat3.out
	./bench/tiers "Map Bool Bool"    > tests/diff/tiers-MapBoolBool.out
	./bench/tiers "Map () ()"        > tests/diff/tiers-MapUU.out
	./bench/tiers "Map Nat Nat"      > tests/diff/tiers-MapNatNat.out
	./bench/tiers "Map Nat2 Nat2"    > tests/diff/tiers-MapNat2Nat2.out
	./bench/tiers "Map Nat3 Nat3"    > tests/diff/tiers-MapNat3Nat3.out

prepare-depend: bench/tiers-listsofpairs.hs \
                bench/tiers-colistable.hs \
                bench/tiers-funlistable.hs \
                bench/tiers-mixed.hs

TLF = "import\ Test.LeanCheck.Function"

bench/tiers-listsofpairs.hs: bench/tiers.hs
	sed -e "s/$(TLF)$$/$(TLF).Listable.ListsOfPairs\n$(TLF).Show/" $< > $@

bench/tiers-colistable.hs: bench/tiers.hs
	sed -e "s/$(TLF)$$/$(TLF).Listable.CoListable\n$(TLF).Show/" $< > $@

bench/tiers-funlistable.hs: bench/tiers.hs
	sed -e "s/$(TLF)$$/$(TLF).Listable.FunListable\n$(TLF).Show/" $< > $@

bench/tiers-mixed.hs: bench/tiers.hs
	sed -e "s/$(TLF)$$/$(TLF).Listable.Mixed\n$(TLF).Show/" $< > $@

bench/tiers-colistable:   bench/tiers-colistable.hs   src/Test/LeanCheck/Function/Listable/CoListable.hs

bench/tiers-listsofpairs: bench/tiers-listsofpairs.hs src/Test/LeanCheck/Function/Listable/ListsOfPairs.hs

bench/tiers-funlistable:  bench/tiers-funlistable.hs  src/Test/LeanCheck/Function/Listable/FunListable.hs

bench/tiers-mixed:        bench/tiers-mixed.hs        src/Test/LeanCheck/Function/Listable/Mixed.hs

diff-test-funtiers: bench/tiers-listsofpairs.diff-test \
                    bench/tiers-funlistable.diff-test \
                    bench/tiers-colistable.diff-test \
                    bench/tiers-mixed.diff-test

update-diff-test-funtiers: bench/tiers-listsofpairs.update-diff-test \
                           bench/tiers-funlistable.update-diff-test \
                           bench/tiers-colistable.update-diff-test \
                           bench/tiers-mixed.update-diff-test

bench/tiers-%.diff-test: bench/tiers-%
	# functions
	$< "()->()"           | diff -rud tests/diff/tiers-$*-U-U.out         -
	$< "Bool->Bool"       | diff -rud tests/diff/tiers-$*-Bool-Bool.out   -
	$< "Bool->()"         | diff -rud tests/diff/tiers-$*-Bool-U.out      -
	$< "()->Bool"         | diff -rud tests/diff/tiers-$*-U-Bool.out      -
	$< "Int->Int"       6 | diff -rud tests/diff/tiers-$*-Int-Int.out     -
	$< "Nat->Nat"       6 | diff -rud tests/diff/tiers-$*-Nat-Nat.out     -
	$< "()->Nat"        6 | diff -rud tests/diff/tiers-$*-U-Nat.out       -
	$< "Nat->()"        6 | diff -rud tests/diff/tiers-$*-Nat-U.out       -
	$< "Int->Int->Int"  4 | diff -rud tests/diff/tiers-$*-Int-Int-Int.out -
	$< "Nat->Nat->Nat"  4 | diff -rud tests/diff/tiers-$*-Nat-Nat-Nat.out -
	$< "(Nat,Nat)->Nat" 4 | diff -rud tests/diff/tiers-$*-Nat,Nat-Nat.out -
	$< "Maybe Bool->Bool" | diff -rud tests/diff/tiers-$*-MBool-Bool.out  -
	$< "Bool->Maybe Bool" | diff -rud tests/diff/tiers-$*-Bool-MBool.out  -
	$< "Maybe Bool->Maybe Bool" | diff -rud tests/diff/tiers-$*-MBool-MBool.out -
	# functions of lists
	$< "[Bool]->[Bool]" 4 | diff -rud tests/diff/tiers-$*-Bools-Bools.out -
	$< "[Nat]->[Nat]"   4 | diff -rud tests/diff/tiers-$*-Nats-Nats.out   -
	$< "[Int]->[Int]"   4 | diff -rud tests/diff/tiers-$*-Ints-Ints.out   -
	# more functions
	$< "Nat2->Nat2"       | diff -rud tests/diff/tiers-$*-Nat2-Nat2.out   -
	$< "Nat2->Nat3"       | diff -rud tests/diff/tiers-$*-Nat2-Nat3.out   -
	$< "Nat3->Nat2"       | diff -rud tests/diff/tiers-$*-Nat3-Nat2.out   -
	$< "Nat3->Nat3"       | diff -rud tests/diff/tiers-$*-Nat3-Nat3.out   -

bench/tiers-%.update-diff-test: bench/tiers-%
	# functions
	$< "()->()"           > tests/diff/tiers-$*-U-U.out
	$< "Bool->Bool"       > tests/diff/tiers-$*-Bool-Bool.out
	$< "Bool->()"         > tests/diff/tiers-$*-Bool-U.out
	$< "()->Bool"         > tests/diff/tiers-$*-U-Bool.out
	$< "Int->Int"       6 > tests/diff/tiers-$*-Int-Int.out
	$< "Nat->Nat"       6 > tests/diff/tiers-$*-Nat-Nat.out
	$< "Nat->()"        6 > tests/diff/tiers-$*-Nat-U.out
	$< "()->Nat"        6 > tests/diff/tiers-$*-U-Nat.out
	$< "Int->Int->Int"  4 > tests/diff/tiers-$*-Int-Int-Int.out
	$< "Nat->Nat->Nat"  4 > tests/diff/tiers-$*-Nat-Nat-Nat.out
	$< "(Nat,Nat)->Nat" 4 > tests/diff/tiers-$*-Nat,Nat-Nat.out
	$< "Maybe Bool->Bool" > tests/diff/tiers-$*-MBool-Bool.out
	$< "Bool->Maybe Bool" > tests/diff/tiers-$*-Bool-MBool.out
	$< "Maybe Bool->Maybe Bool" > tests/diff/tiers-$*-MBool-MBool.out
	# functions of lists
	$< "[()]->[()]"     4 > tests/diff/tiers-$*-Us-Us.out
	$< "[Bool]->[Bool]" 4 > tests/diff/tiers-$*-Bools-Bools.out
	$< "[Nat]->[Nat]"   4 > tests/diff/tiers-$*-Nats-Nats.out
	$< "[Int]->[Int]"   4 > tests/diff/tiers-$*-Ints-Ints.out
	# more functions
	$< "Nat2->Nat2"       > tests/diff/tiers-$*-Nat2-Nat2.out
	$< "Nat2->Nat3"       > tests/diff/tiers-$*-Nat2-Nat3.out
	$< "Nat3->Nat2"       > tests/diff/tiers-$*-Nat3-Nat2.out
	$< "Nat3->Nat3"       > tests/diff/tiers-$*-Nat3-Nat3.out
