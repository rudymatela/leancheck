# Makefile for LeanCheck
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = test/main      \
        test/derive    \
        test/error     \
        test/fun       \
        test/funshow   \
        test/generic   \
        test/io        \
        test/operators \
        test/tiers     \
        test/stats     \
        test/types
EGS = \
	eg/overflow \
	eg/higher-order \
	eg/test-bool \
	eg/test-float \
	eg/test-list \
	eg/test-sort
BENCHS = \
	bench/tiers-default \
	bench/tiers-4cases \
	bench/tiers
GHCIMPORTDIRS = src:test
# -dynamic is needed only for src/Test/LeanCheck/Derive.hs and test/derive.hs
GHCFLAGS = -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HADDOCKFLAGS = --no-print-missing-docs
HUGSIMPORTDIRS = .:./src:./test:./etc/hugs-backports:/usr/lib/hugs/packages/*
HUGSFLAGS = -98 -h32M

all: mk/toplibs

all-all: mk/All.o

test: $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

diff-test: diff-test-tiers diff-test-funtiers $(patsubst %,%.diff-test,$(EGS))

update-diff-test: update-diff-test-tiers update-diff-test-funtiers $(patsubst %,%.update-diff-test,$(EGS))

%.run: %
	./$<

eg/%.diff-test: eg/%
	./$< | diff -rud test/diff/$<.out -

eg/%.update-diff-test: eg/%
	./$< >           test/diff/$<.out

# Evaluation order changed from GHC 8.4 to GHC 8.6, so we need to skip the
# contents of the exception for test-list.diff-test.
eg/test-list.diff-test: eg/test-list
	./$< | sed -e "s/Exception '[^']*'/Exception '...'/" | diff -rud test/diff/$<.out -

eg/test-list.update-diff-test: eg/test-list
	./$< | sed -e "s/Exception '[^']*'/Exception '...'/" >           test/diff/$<.out

clean: clean-hi-o clean-haddock
	rm -f bench/tiers-default.hs
	rm -f bench/tiers-4cases.hs
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

full-clean: clean
	rm -rf .stack-work
	rm -f tags TAGS

ghci: mk/All.ghci

hugs: src/Test/LeanCheck.hugs

hugs-test: \
  test/fun.runhugs \
  test/io.runhugs \
  test/operators.runhugs \
  test/stats.runhugs \
  test/tiers.runhugs \
  test/main.runhugs
# test/error.runhugs    # TODO: make this pass
# test/funshow.runhugs  # TODO: make this pass
# test/types.runhugs    # TODO: make this pass

install:
	@echo "use \`cabal install' instead"

test-sdist:
	./test/sdist

test-via-cabal:
	cabal test

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

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk

diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"               | diff -rud test/diff/tiers.out             -
	./bench/tiers "Int"              | diff -rud test/diff/tiers-Int.out         -
	./bench/tiers "Nat"              | diff -rud test/diff/tiers-Nat.out         -
	./bench/tiers "Integer"          | diff -rud test/diff/tiers-Integer.out     -
	./bench/tiers "Bool"             | diff -rud test/diff/tiers-Bool.out        -
	./bench/tiers "Char"             | diff -rud test/diff/tiers-Char.out        -
	./bench/tiers "Float"            | diff -rud test/diff/tiers-Float.out       -
	./bench/tiers "Double"           | diff -rud test/diff/tiers-Double.out      -
	./bench/tiers "Rational"         | diff -rud test/diff/tiers-Rational.out    -
	# fixed width integer types
	./bench/tiers "Nat2"             | diff -rud test/diff/tiers-Nat2.out        -
	./bench/tiers "Nat3"             | diff -rud test/diff/tiers-Nat3.out        -
	./bench/tiers "Nat4"             | diff -rud test/diff/tiers-Nat4.out        -
	./bench/tiers "Word2"            | diff -rud test/diff/tiers-Word2.out       -
	./bench/tiers "Word3"            | diff -rud test/diff/tiers-Word3.out       -
	./bench/tiers "Word4"         64 | diff -rud test/diff/tiers-Word4.out       -
	./bench/tiers "Word8"        256 | diff -rud test/diff/tiers-Word8.out       -
	./bench/tiers "Int2"             | diff -rud test/diff/tiers-Int2.out        -
	./bench/tiers "Int3"             | diff -rud test/diff/tiers-Int3.out        -
	./bench/tiers "Int4"          64 | diff -rud test/diff/tiers-Int4.out        -
	./bench/tiers "Int8"         256 | diff -rud test/diff/tiers-Int8.out        -
	# list s
	./bench/tiers "[()]"             | diff -rud test/diff/tiers-Us.out          -
	./bench/tiers "[Int]"          6 | diff -rud test/diff/tiers-Ints.out        -
	./bench/tiers "[Nat]"          6 | diff -rud test/diff/tiers-Nats.out        -
	./bench/tiers "[Bool]"         6 | diff -rud test/diff/tiers-Bools.out       -
	./bench/tiers "String"         6 | diff -rud test/diff/tiers-String.out      -
	# pairs
	./bench/tiers "(Int,Int)"        | diff -rud test/diff/tiers-Int,Int.out     -
	./bench/tiers "(Nat,Nat)"        | diff -rud test/diff/tiers-Nat,Nat.out     -
	./bench/tiers "(Int,Int,Int)"  6 | diff -rud test/diff/tiers-Int,Int,Int.out -
	./bench/tiers "(Nat,Nat,Nat)"  6 | diff -rud test/diff/tiers-Nat,Nat,Nat.out -
	# lists & pairs
	./bench/tiers "[((),())]"        | diff -rud test/diff/tiers-U,Us.out        -
	./bench/tiers "([()],[()])"      | diff -rud test/diff/tiers-Us,Us.out       -
	# special lists
	./bench/tiers "Set Bool"         | diff -rud test/diff/tiers-SetBool.out     -
	./bench/tiers "Set ()"           | diff -rud test/diff/tiers-SetU.out        -
	./bench/tiers "Set Nat"          | diff -rud test/diff/tiers-SetNat.out      -
	./bench/tiers "Set Nat2"         | diff -rud test/diff/tiers-SetNat2.out     -
	./bench/tiers "Set Nat3"         | diff -rud test/diff/tiers-SetNat3.out     -
	./bench/tiers "Bag Bool"         | diff -rud test/diff/tiers-BagBool.out     -
	./bench/tiers "Bag ()"           | diff -rud test/diff/tiers-BagU.out        -
	./bench/tiers "Bag Nat"          | diff -rud test/diff/tiers-BagNat.out      -
	./bench/tiers "Bag Nat2"         | diff -rud test/diff/tiers-BagNat2.out     -
	./bench/tiers "Bag Nat3"         | diff -rud test/diff/tiers-BagNat3.out     -
	./bench/tiers "NoDup Bool"       | diff -rud test/diff/tiers-NoDupBool.out   -
	./bench/tiers "NoDup ()"         | diff -rud test/diff/tiers-NoDupU.out      -
	./bench/tiers "NoDup Nat"        | diff -rud test/diff/tiers-NoDupNat.out    -
	./bench/tiers "NoDup Nat2"       | diff -rud test/diff/tiers-NoDupNat2.out   -
	./bench/tiers "NoDup Nat3"       | diff -rud test/diff/tiers-NoDupNat3.out   -
	./bench/tiers "Map Bool Bool"    | diff -rud test/diff/tiers-MapBoolBool.out -
	./bench/tiers "Map () ()"        | diff -rud test/diff/tiers-MapUU.out       -
	./bench/tiers "Map Nat Nat"      | diff -rud test/diff/tiers-MapNatNat.out   -
	./bench/tiers "Map Nat2 Nat2"    | diff -rud test/diff/tiers-MapNat2Nat2.out -
	./bench/tiers "Map Nat3 Nat3"    | diff -rud test/diff/tiers-MapNat3Nat3.out -
	# extreme integers
	./bench/tiers "X Int4"           | diff -rud test/diff/tiers-XInt4.out       -
	./bench/tiers "X Word4"          | diff -rud test/diff/tiers-XWord4.out      -
	./bench/tiers "X Nat7"           | diff -rud test/diff/tiers-XNat7.out       -
	./bench/tiers "Xs Int4"          | diff -rud test/diff/tiers-XsInt4.out      -
	./bench/tiers "Xs Word4"         | diff -rud test/diff/tiers-XsWord4.out     -
	./bench/tiers "Xs Nat7"          | diff -rud test/diff/tiers-XsNat7.out      -

update-diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"               > test/diff/tiers.out
	./bench/tiers "Int"              > test/diff/tiers-Int.out
	./bench/tiers "Nat"              > test/diff/tiers-Nat.out
	./bench/tiers "Integer"          > test/diff/tiers-Integer.out
	./bench/tiers "Bool"             > test/diff/tiers-Bool.out
	./bench/tiers "Char"             > test/diff/tiers-Char.out
	./bench/tiers "Float"            > test/diff/tiers-Float.out
	./bench/tiers "Double"           > test/diff/tiers-Double.out
	./bench/tiers "Rational"         > test/diff/tiers-Rational.out
	# fixed width integer types
	./bench/tiers "Nat2"             > test/diff/tiers-Nat2.out
	./bench/tiers "Nat3"             > test/diff/tiers-Nat3.out
	./bench/tiers "Nat4"             > test/diff/tiers-Nat4.out
	./bench/tiers "Word2"            > test/diff/tiers-Word2.out
	./bench/tiers "Word3"            > test/diff/tiers-Word3.out
	./bench/tiers "Word4"         64 > test/diff/tiers-Word4.out
	./bench/tiers "Word8"        256 > test/diff/tiers-Word8.out
	./bench/tiers "Int2"             > test/diff/tiers-Int2.out
	./bench/tiers "Int3"             > test/diff/tiers-Int3.out
	./bench/tiers "Int4"          64 > test/diff/tiers-Int4.out
	./bench/tiers "Int8"         256 > test/diff/tiers-Int8.out
	# lists
	./bench/tiers "[()]"             > test/diff/tiers-Us.out
	./bench/tiers "[Int]"          6 > test/diff/tiers-Ints.out
	./bench/tiers "[Nat]"          6 > test/diff/tiers-Nats.out
	./bench/tiers "[Bool]"         6 > test/diff/tiers-Bools.out
	./bench/tiers "String"         6 > test/diff/tiers-String.out
	# pairs
	./bench/tiers "(Int,Int)"        > test/diff/tiers-Int,Int.out
	./bench/tiers "(Nat,Nat)"        > test/diff/tiers-Nat,Nat.out
	./bench/tiers "(Int,Int,Int)"  6 > test/diff/tiers-Int,Int,Int.out
	./bench/tiers "(Nat,Nat,Nat)"  6 > test/diff/tiers-Nat,Nat,Nat.out
	# lists & pairs
	./bench/tiers "[((),())]"        > test/diff/tiers-U,Us.out
	./bench/tiers "([()],[()])"      > test/diff/tiers-Us,Us.out
	# special lists
	./bench/tiers "Set Bool"         > test/diff/tiers-SetBool.out
	./bench/tiers "Set ()"           > test/diff/tiers-SetU.out
	./bench/tiers "Set Nat"          > test/diff/tiers-SetNat.out
	./bench/tiers "Set Nat2"         > test/diff/tiers-SetNat2.out
	./bench/tiers "Set Nat3"         > test/diff/tiers-SetNat3.out
	./bench/tiers "Bag Bool"         > test/diff/tiers-BagBool.out
	./bench/tiers "Bag ()"           > test/diff/tiers-BagU.out
	./bench/tiers "Bag Nat"          > test/diff/tiers-BagNat.out
	./bench/tiers "Bag Nat2"         > test/diff/tiers-BagNat2.out
	./bench/tiers "Bag Nat3"         > test/diff/tiers-BagNat3.out
	./bench/tiers "NoDup Bool"       > test/diff/tiers-NoDupBool.out
	./bench/tiers "NoDup ()"         > test/diff/tiers-NoDupU.out
	./bench/tiers "NoDup Nat"        > test/diff/tiers-NoDupNat.out
	./bench/tiers "NoDup Nat2"       > test/diff/tiers-NoDupNat2.out
	./bench/tiers "NoDup Nat3"       > test/diff/tiers-NoDupNat3.out
	./bench/tiers "Map Bool Bool"    > test/diff/tiers-MapBoolBool.out
	./bench/tiers "Map () ()"        > test/diff/tiers-MapUU.out
	./bench/tiers "Map Nat Nat"      > test/diff/tiers-MapNatNat.out
	./bench/tiers "Map Nat2 Nat2"    > test/diff/tiers-MapNat2Nat2.out
	./bench/tiers "Map Nat3 Nat3"    > test/diff/tiers-MapNat3Nat3.out
	# extreme integers
	./bench/tiers "X Int4"           > test/diff/tiers-XInt4.out
	./bench/tiers "X Word4"          > test/diff/tiers-XWord4.out
	./bench/tiers "X Nat7"           > test/diff/tiers-XNat7.out
	./bench/tiers "Xs Int4"          > test/diff/tiers-XsInt4.out
	./bench/tiers "Xs Word4"         > test/diff/tiers-XsWord4.out
	./bench/tiers "Xs Nat7"          > test/diff/tiers-XsNat7.out

prepare-depend: bench/tiers-default.hs \
                bench/tiers-4cases.hs

prepare-depend-and-depend: prepare-depend
	make depend

TLF = "import\ Test.LeanCheck.Function"

bench/tiers-default.hs: bench/tiers.hs
	cp $< $@

bench/tiers-4cases.hs: bench/tiers.hs
	sed -e "s/$(TLF)$$/$(TLF).Listable\n$(TLF).Show.FourCases/" $< > $@

bench/tiers-default: bench/tiers-default.hs src/Test/LeanCheck/Function/Listable/ListsOfPairs.hs

bench/tiers-4cases: bench/tiers-4cases.hs

diff-test-funtiers: bench/tiers-default.diff-test \
                    bench/tiers-4cases.diff-test

update-diff-test-funtiers: bench/tiers-default.update-diff-test \
                           bench/tiers-4cases.update-diff-test

bench/tiers-%.diff-test: bench/tiers-%
	# functions
	$< "()->()"           | diff -rud test/diff/tiers-$*-U-U.out         -
	$< "Bool->Bool"       | diff -rud test/diff/tiers-$*-Bool-Bool.out   -
	$< "Bool->Bool->Bool" | diff -rud test/diff/tiers-$*-Bool-Bool-Bool.out -
	$< "Bool->()"         | diff -rud test/diff/tiers-$*-Bool-U.out      -
	$< "()->Bool"         | diff -rud test/diff/tiers-$*-U-Bool.out      -
	$< "Int->Int"       9 | diff -rud test/diff/tiers-$*-Int-Int.out     -
	$< "Nat->Nat"       9 | diff -rud test/diff/tiers-$*-Nat-Nat.out     -
	$< "()->Nat"        6 | diff -rud test/diff/tiers-$*-U-Nat.out       -
	$< "Nat->()"        6 | diff -rud test/diff/tiers-$*-Nat-U.out       -
	$< "Int->Int->Int"  6 | diff -rud test/diff/tiers-$*-Int-Int-Int.out -
	$< "Nat->Nat->Nat"  6 | diff -rud test/diff/tiers-$*-Nat-Nat-Nat.out -
	$< "(Nat,Nat)->Nat" 6 | diff -rud test/diff/tiers-$*-Nat,Nat-Nat.out -
	$< "Maybe Bool->Bool" | diff -rud test/diff/tiers-$*-MBool-Bool.out  -
	$< "Bool->Maybe Bool" | diff -rud test/diff/tiers-$*-Bool-MBool.out  -
	$< "Maybe Bool->Maybe Bool" | diff -rud test/diff/tiers-$*-MBool-MBool.out -
	# functions with mixed arguments
	$< "Bool->Int->Bool" 6 | diff -rud test/diff/tiers-$*-Bool-Int-Bool.out -
	$< "Int->Bool->Bool" 6 | diff -rud test/diff/tiers-$*-Int-Bool-Bool.out -
	# functions with 3 arguments
	$< "Int->Int->Int->Int"     4 | diff -rud test/diff/tiers-$*-Int-Int-Int-Int.out -
	$< "Bool->Bool->Bool->Bool"   | diff -rud test/diff/tiers-$*-Bool-Bool-Bool-Bool.out -
	# functions of lists
	$< "[Bool]->[Bool]" 6 | diff -rud test/diff/tiers-$*-Bools-Bools.out -
	$< "[Nat]->[Nat]"   6 | diff -rud test/diff/tiers-$*-Nats-Nats.out   -
	$< "[Int]->[Int]"   6 | diff -rud test/diff/tiers-$*-Ints-Ints.out   -
	# more functions
	$< "Nat2->Nat2"       | diff -rud test/diff/tiers-$*-Nat2-Nat2.out   -
	$< "Nat2->Nat3"       | diff -rud test/diff/tiers-$*-Nat2-Nat3.out   -
	$< "Nat3->Nat2"       | diff -rud test/diff/tiers-$*-Nat3-Nat2.out   -
	$< "Nat3->Nat3"       | diff -rud test/diff/tiers-$*-Nat3-Nat3.out   -

bench/tiers-%.update-diff-test: bench/tiers-%
	# functions
	$< "()->()"           > test/diff/tiers-$*-U-U.out
	$< "Bool->Bool"       > test/diff/tiers-$*-Bool-Bool.out
	$< "Bool->Bool->Bool" > test/diff/tiers-$*-Bool-Bool-Bool.out
	$< "Bool->()"         > test/diff/tiers-$*-Bool-U.out
	$< "()->Bool"         > test/diff/tiers-$*-U-Bool.out
	$< "Int->Int"       9 > test/diff/tiers-$*-Int-Int.out
	$< "Nat->Nat"       9 > test/diff/tiers-$*-Nat-Nat.out
	$< "Nat->()"        6 > test/diff/tiers-$*-Nat-U.out
	$< "()->Nat"        6 > test/diff/tiers-$*-U-Nat.out
	$< "Int->Int->Int"  6 > test/diff/tiers-$*-Int-Int-Int.out
	$< "Nat->Nat->Nat"  6 > test/diff/tiers-$*-Nat-Nat-Nat.out
	$< "(Nat,Nat)->Nat" 6 > test/diff/tiers-$*-Nat,Nat-Nat.out
	$< "Maybe Bool->Bool" > test/diff/tiers-$*-MBool-Bool.out
	$< "Bool->Maybe Bool" > test/diff/tiers-$*-Bool-MBool.out
	$< "Maybe Bool->Maybe Bool" > test/diff/tiers-$*-MBool-MBool.out
	# functions with mixed arguments
	$< "Bool->Int->Bool" 6 > test/diff/tiers-$*-Bool-Int-Bool.out
	$< "Int->Bool->Bool" 6 > test/diff/tiers-$*-Int-Bool-Bool.out
	# functions with 3 arguments
	$< "Int->Int->Int->Int"     4 > test/diff/tiers-$*-Int-Int-Int-Int.out
	$< "Bool->Bool->Bool->Bool"   > test/diff/tiers-$*-Bool-Bool-Bool-Bool.out
	# functions of lists
	$< "[()]->[()]"     6 > test/diff/tiers-$*-Us-Us.out
	$< "[Bool]->[Bool]" 6 > test/diff/tiers-$*-Bools-Bools.out
	$< "[Nat]->[Nat]"   6 > test/diff/tiers-$*-Nats-Nats.out
	$< "[Int]->[Int]"   6 > test/diff/tiers-$*-Ints-Ints.out
	# more functions
	$< "Nat2->Nat2"       > test/diff/tiers-$*-Nat2-Nat2.out
	$< "Nat2->Nat3"       > test/diff/tiers-$*-Nat2-Nat3.out
	$< "Nat3->Nat2"       > test/diff/tiers-$*-Nat3-Nat2.out
	$< "Nat3->Nat3"       > test/diff/tiers-$*-Nat3-Nat3.out
