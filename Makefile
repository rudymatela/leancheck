# Makefile for LeanCheck
#
# Copyright:   (c) 2015-2024 Rudy Matela
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
	bench/dets \
	bench/pick \
	bench/memory-usage \
	bench/tiers-default \
	bench/tiers-4cases \
	bench/tiers
GHCIMPORTDIRS = src:test
GHCFLAGS = -v0 -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic) \
  -W -fno-warn-unused-matches -Werror
HUGSIMPORTDIRS = .:./src:./test:./etc/hugs-backports:/usr/lib/hugs/packages/*
HUGSFLAGS = -98 -h32M

LIB_DEPS = base template-haskell

all: mk/toplibs

all-all: mk/All.o

test: $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

diff-test: diff-tiers diff-funtiers $(patsubst %,%.diff,$(EGS))

txt: txt-tiers txt-funtiers $(patsubst %,%.txt,$(EGS))

test-10000: $(patsubst %,%.run-10000,$(TESTS)) diff-test test-sdist

%.run: %
	./$<

%.run-10000: %
	./$< 10000

%.txt: %
	./$< >$@

%.diff: %
	./$< | diff $<.txt -

# Evaluation order changed from GHC 8.4 to GHC 8.6, so we need to skip the
# contents of the exception for test-list.diff-test.
# Exception is multiline starting with GHC 9.4, hence the 'tr' trick
hide_exceptions = tr '\n' '\0' | sed -e "s/Exception '[^']*'/Exception '...'/" | tr '\0' '\n'

eg/test-list.txt: eg/test-list
	./$< | $(hide_exceptions) > $@

eg/test-list.diff: eg/test-list
	./$< | $(hide_exceptions) | diff $<.txt -

clean: clean-hi-o clean-haddock
	rm -f bench/tiers-default.hs
	rm -f bench/tiers-4cases.hs
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

full-clean: clean clean-cabal clean-stack
	rm -f tags TAGS

ghci: mk/All.ghci

hugs: src/Test/LeanCheck.hugs

hugs-test: \
  test/main.runhugs \
  test/fun.runhugs \
  test/funshow.runhugs \
  test/io.runhugs \
  test/operators.runhugs \
  test/stats.runhugs \
  test/tiers.runhugs \
  test/types.runhugs \
  test/error.runhugs

install:
	@echo "use \`cabal install' instead"

test-sdist:
	./test/sdist

test-via-cabal:
	cabal update # recent (2024) cabal refuses to build without a package list
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test main

test-via-stack:
	stack test leancheck:test:main --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-with-ghc-7.6:
	make clean
	make GHC=ghc-7.6 GHCFLAGS=-dynamic test/{main,fun,funshow,io,operators,stats,tiers,types,error}.run -j9

legacy-test: # needs ghc-9.4 .. ghc-7.8 installed as such
	make clean  &&  make test -j GHC=ghc-9.4
	make clean  &&  make test -j GHC=ghc-9.2
	make clean  &&  make test -j GHC=ghc-9.0
	make clean  &&  make test -j GHC=ghc-8.8
	make clean  &&  make test -j GHC=ghc-8.6
	make clean  &&  make test -j GHC=ghc-8.4
	make clean  &&  make test -j GHC=ghc-8.2
	make clean  &&  make test -j GHC=ghc-8.0
	make clean  &&  make test -j GHC=ghc-7.10
	make clean  &&  make test -j GHC=ghc-7.8
	make clean  &&  make test -j

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-9.4  configure  &&  cabal-ghc-9.4  test
	cabal clean  &&  cabal-ghc-9.2  configure  &&  cabal-ghc-9.2  test
	cabal clean  &&  cabal-ghc-9.0  configure  &&  cabal-ghc-9.0  test
	cabal clean  &&  cabal-ghc-8.8  configure  &&  cabal-ghc-8.8  test
	cabal clean  &&  cabal-ghc-8.6  configure  &&  cabal-ghc-8.6  test
	cabal clean  &&  cabal-ghc-8.4  configure  &&  cabal-ghc-8.4  test
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

hlint: ..hlint # src.hlint eg.hlint test.hlint bench.hlint

# use "make src.hlint" to run hlint only on the src folder
%.hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  --ignore "Redundant lambda" \
	  --ignore "Use lambda-case" \
	  --ignore "Redundant ==" \
	  --ignore "Use isNothing" \
	  $*

markdown: \
  README.html \
  doc/tutorial.html \
  doc/data-invariant.html \
  doc/faq.html

%.html: %.md
	pandoc $< -o $@

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

# list lines that are longer than 80 characters
d=..........
e=$d$d$d$d$d$d$d$d
list-80:
	find src test bench eg -name "*.hs" | xargs grep -n --color -R "^$e." || true

# list = definitions surrounded by a single space
list-single-space-equals:
	find src test bench eg -name "*.hs" | xargs grep -n --color -R "[^ ] = [^ ]" || true

list-missing-copyright:
	find src test bench eg -name "*.hs" | xargs grep -LR Copyright || true

include mk/haskell.mk

diff-tiers: bench/tiers
	# simple types
	$< "()"              | diff -rud bench/tiers-txt/U.txt           -
	$< "Int"             | diff -rud bench/tiers-txt/Int.txt         -
	$< "Nat"             | diff -rud bench/tiers-txt/Nat.txt         -
	$< "Integer"         | diff -rud bench/tiers-txt/Integer.txt     -
	$< "Bool"            | diff -rud bench/tiers-txt/Bool.txt        -
	$< "Char"            | diff -rud bench/tiers-txt/Char.txt        -
	$< "Float"           | diff -rud bench/tiers-txt/Float.txt       -
	$< "Double"          | diff -rud bench/tiers-txt/Double.txt      -
	$< "Rational"        | diff -rud bench/tiers-txt/Rational.txt    -
	# fixed width integer types
	$< "Nat2"            | diff -rud bench/tiers-txt/Nat2.txt        -
	$< "Nat3"            | diff -rud bench/tiers-txt/Nat3.txt        -
	$< "Nat4"            | diff -rud bench/tiers-txt/Nat4.txt        -
	$< "Word2"           | diff -rud bench/tiers-txt/Word2.txt       -
	$< "Word3"           | diff -rud bench/tiers-txt/Word3.txt       -
	$< "Word4"        64 | diff -rud bench/tiers-txt/Word4.txt       -
	$< "Word8"       256 | diff -rud bench/tiers-txt/Word8.txt       -
	$< "Int2"            | diff -rud bench/tiers-txt/Int2.txt        -
	$< "Int3"            | diff -rud bench/tiers-txt/Int3.txt        -
	$< "Int4"         64 | diff -rud bench/tiers-txt/Int4.txt        -
	$< "Int8"        256 | diff -rud bench/tiers-txt/Int8.txt        -
	# complex numbers
	$< "Complex Double"  | diff -rud bench/tiers-txt/ComplexDouble.txt -
	# list s
	$< "[()]"            | diff -rud bench/tiers-txt/Us.txt          -
	$< "[Int]"         6 | diff -rud bench/tiers-txt/Ints.txt        -
	$< "[Nat]"         6 | diff -rud bench/tiers-txt/Nats.txt        -
	$< "[Bool]"        6 | diff -rud bench/tiers-txt/Bools.txt       -
	$< "String"        6 | diff -rud bench/tiers-txt/String.txt      -
	# pairs
	$< "(Int,Int)"       | diff -rud bench/tiers-txt/Int,Int.txt     -
	$< "(Nat,Nat)"       | diff -rud bench/tiers-txt/Nat,Nat.txt     -
	$< "(Int,Int,Int)" 6 | diff -rud bench/tiers-txt/Int,Int,Int.txt -
	$< "(Nat,Nat,Nat)" 6 | diff -rud bench/tiers-txt/Nat,Nat,Nat.txt -
	# lists & pairs
	$< "[((),())]"       | diff -rud bench/tiers-txt/U,Us.txt        -
	$< "([()],[()])"     | diff -rud bench/tiers-txt/Us,Us.txt       -
	# special lists
	$< "Set Bool"        | diff -rud bench/tiers-txt/SetBool.txt     -
	$< "Set ()"          | diff -rud bench/tiers-txt/SetU.txt        -
	$< "Set Nat"         | diff -rud bench/tiers-txt/SetNat.txt      -
	$< "Set Nat2"        | diff -rud bench/tiers-txt/SetNat2.txt     -
	$< "Set Nat3"        | diff -rud bench/tiers-txt/SetNat3.txt     -
	$< "Bag Bool"        | diff -rud bench/tiers-txt/BagBool.txt     -
	$< "Bag ()"          | diff -rud bench/tiers-txt/BagU.txt        -
	$< "Bag Nat"         | diff -rud bench/tiers-txt/BagNat.txt      -
	$< "Bag Nat2"        | diff -rud bench/tiers-txt/BagNat2.txt     -
	$< "Bag Nat3"        | diff -rud bench/tiers-txt/BagNat3.txt     -
	$< "NoDup Bool"      | diff -rud bench/tiers-txt/NoDupBool.txt   -
	$< "NoDup ()"        | diff -rud bench/tiers-txt/NoDupU.txt      -
	$< "NoDup Nat"       | diff -rud bench/tiers-txt/NoDupNat.txt    -
	$< "NoDup Nat2"      | diff -rud bench/tiers-txt/NoDupNat2.txt   -
	$< "NoDup Nat3"      | diff -rud bench/tiers-txt/NoDupNat3.txt   -
	$< "Map Bool Bool"   | diff -rud bench/tiers-txt/MapBoolBool.txt -
	$< "Map () ()"       | diff -rud bench/tiers-txt/MapUU.txt       -
	$< "Map Nat Nat"     | diff -rud bench/tiers-txt/MapNatNat.txt   -
	$< "Map Nat2 Nat2"   | diff -rud bench/tiers-txt/MapNat2Nat2.txt -
	$< "Map Nat3 Nat3"   | diff -rud bench/tiers-txt/MapNat3Nat3.txt -
	# extreme integers
	$< "X Int4"          | diff -rud bench/tiers-txt/XInt4.txt       -
	$< "X Word4"         | diff -rud bench/tiers-txt/XWord4.txt      -
	$< "X Nat7"          | diff -rud bench/tiers-txt/XNat7.txt       -
	$< "Xs Int4"         | diff -rud bench/tiers-txt/XsInt4.txt      -
	$< "Xs Word4"        | diff -rud bench/tiers-txt/XsWord4.txt     -
	$< "Xs Nat7"         | diff -rud bench/tiers-txt/XsNat7.txt      -

txt-tiers: bench/tiers
	# simple types
	$< "()"              > bench/tiers-txt/U.txt
	$< "Int"             > bench/tiers-txt/Int.txt
	$< "Nat"             > bench/tiers-txt/Nat.txt
	$< "Integer"         > bench/tiers-txt/Integer.txt
	$< "Bool"            > bench/tiers-txt/Bool.txt
	$< "Char"            > bench/tiers-txt/Char.txt
	$< "Float"           > bench/tiers-txt/Float.txt
	$< "Double"          > bench/tiers-txt/Double.txt
	$< "Rational"        > bench/tiers-txt/Rational.txt
	# fixed width integer types
	$< "Nat2"            > bench/tiers-txt/Nat2.txt
	$< "Nat3"            > bench/tiers-txt/Nat3.txt
	$< "Nat4"            > bench/tiers-txt/Nat4.txt
	$< "Word2"           > bench/tiers-txt/Word2.txt
	$< "Word3"           > bench/tiers-txt/Word3.txt
	$< "Word4"        64 > bench/tiers-txt/Word4.txt
	$< "Word8"       256 > bench/tiers-txt/Word8.txt
	$< "Int2"            > bench/tiers-txt/Int2.txt
	$< "Int3"            > bench/tiers-txt/Int3.txt
	$< "Int4"         64 > bench/tiers-txt/Int4.txt
	$< "Int8"        256 > bench/tiers-txt/Int8.txt
	# complex numbers
	$< "Complex Double"  > bench/tiers-txt/ComplexDouble.txt
	# lists
	$< "[()]"            > bench/tiers-txt/Us.txt
	$< "[Int]"         6 > bench/tiers-txt/Ints.txt
	$< "[Nat]"         6 > bench/tiers-txt/Nats.txt
	$< "[Bool]"        6 > bench/tiers-txt/Bools.txt
	$< "String"        6 > bench/tiers-txt/String.txt
	# pairs
	$< "(Int,Int)"       > bench/tiers-txt/Int,Int.txt
	$< "(Nat,Nat)"       > bench/tiers-txt/Nat,Nat.txt
	$< "(Int,Int,Int)" 6 > bench/tiers-txt/Int,Int,Int.txt
	$< "(Nat,Nat,Nat)" 6 > bench/tiers-txt/Nat,Nat,Nat.txt
	# lists & pairs
	$< "[((),())]"       > bench/tiers-txt/U,Us.txt
	$< "([()],[()])"     > bench/tiers-txt/Us,Us.txt
	# special lists
	$< "Set Bool"        > bench/tiers-txt/SetBool.txt
	$< "Set ()"          > bench/tiers-txt/SetU.txt
	$< "Set Nat"         > bench/tiers-txt/SetNat.txt
	$< "Set Nat2"        > bench/tiers-txt/SetNat2.txt
	$< "Set Nat3"        > bench/tiers-txt/SetNat3.txt
	$< "Bag Bool"        > bench/tiers-txt/BagBool.txt
	$< "Bag ()"          > bench/tiers-txt/BagU.txt
	$< "Bag Nat"         > bench/tiers-txt/BagNat.txt
	$< "Bag Nat2"        > bench/tiers-txt/BagNat2.txt
	$< "Bag Nat3"        > bench/tiers-txt/BagNat3.txt
	$< "NoDup Bool"      > bench/tiers-txt/NoDupBool.txt
	$< "NoDup ()"        > bench/tiers-txt/NoDupU.txt
	$< "NoDup Nat"       > bench/tiers-txt/NoDupNat.txt
	$< "NoDup Nat2"      > bench/tiers-txt/NoDupNat2.txt
	$< "NoDup Nat3"      > bench/tiers-txt/NoDupNat3.txt
	$< "Map Bool Bool"   > bench/tiers-txt/MapBoolBool.txt
	$< "Map () ()"       > bench/tiers-txt/MapUU.txt
	$< "Map Nat Nat"     > bench/tiers-txt/MapNatNat.txt
	$< "Map Nat2 Nat2"   > bench/tiers-txt/MapNat2Nat2.txt
	$< "Map Nat3 Nat3"   > bench/tiers-txt/MapNat3Nat3.txt
	# extreme integers
	$< "X Int4"          > bench/tiers-txt/XInt4.txt
	$< "X Word4"         > bench/tiers-txt/XWord4.txt
	$< "X Nat7"          > bench/tiers-txt/XNat7.txt
	$< "Xs Int4"         > bench/tiers-txt/XsInt4.txt
	$< "Xs Word4"        > bench/tiers-txt/XsWord4.txt
	$< "Xs Nat7"         > bench/tiers-txt/XsNat7.txt

prepare-depend: bench/tiers-default.hs \
                bench/tiers-4cases.hs

prepare-depend-and-depend: prepare-depend
	make depend

TLF = import Test.LeanCheck.Function

bench/tiers-default.hs: bench/tiers.hs
	cp $< $@

bench/tiers-4cases.hs: bench/tiers.hs
	sed -e "s/$(TLF) .*$$/$(TLF).Listable ()\n$(TLF).Show.FourCases ()/" $< > $@

bench/tiers-default: bench/tiers-default.hs src/Test/LeanCheck/Function/Listable/ListsOfPairs.hs

bench/tiers-4cases: bench/tiers-4cases.hs

diff-funtiers: bench/tiers-default.diff \
               bench/tiers-4cases.diff

txt-funtiers: bench/tiers-default.txt \
              bench/tiers-4cases.txt

bench/tiers-%.diff: bench/tiers-%
	# functions
	$< "()->()"           | diff -rud bench/tiers-txt/$*-U-U.txt         -
	$< "Bool->Bool"       | diff -rud bench/tiers-txt/$*-Bool-Bool.txt   -
	$< "Bool->Bool->Bool" | diff -rud bench/tiers-txt/$*-Bool-Bool-Bool.txt -
	$< "Bool->()"         | diff -rud bench/tiers-txt/$*-Bool-U.txt      -
	$< "()->Bool"         | diff -rud bench/tiers-txt/$*-U-Bool.txt      -
	$< "Int->Int"       9 | diff -rud bench/tiers-txt/$*-Int-Int.txt     -
	$< "Nat->Nat"       9 | diff -rud bench/tiers-txt/$*-Nat-Nat.txt     -
	$< "()->Nat"        6 | diff -rud bench/tiers-txt/$*-U-Nat.txt       -
	$< "Nat->()"        6 | diff -rud bench/tiers-txt/$*-Nat-U.txt       -
	$< "Int->Int->Int"  6 | diff -rud bench/tiers-txt/$*-Int-Int-Int.txt -
	$< "Nat->Nat->Nat"  6 | diff -rud bench/tiers-txt/$*-Nat-Nat-Nat.txt -
	$< "(Nat,Nat)->Nat" 6 | diff -rud bench/tiers-txt/$*-Nat,Nat-Nat.txt -
	$< "Maybe Bool->Bool" | diff -rud bench/tiers-txt/$*-MBool-Bool.txt  -
	$< "Bool->Maybe Bool" | diff -rud bench/tiers-txt/$*-Bool-MBool.txt  -
	$< "Maybe Bool->Maybe Bool" | diff -rud bench/tiers-txt/$*-MBool-MBool.txt -
	# functions with mixed arguments
	$< "Bool->Int->Bool" 6 | diff -rud bench/tiers-txt/$*-Bool-Int-Bool.txt -
	$< "Int->Bool->Bool" 6 | diff -rud bench/tiers-txt/$*-Int-Bool-Bool.txt -
	# functions with 3 arguments
	$< "Int->Int->Int->Int"     4 | diff -rud bench/tiers-txt/$*-Int-Int-Int-Int.txt -
	$< "Bool->Bool->Bool->Bool"   | diff -rud bench/tiers-txt/$*-Bool-Bool-Bool-Bool.txt -
	# functions of lists
	$< "[Bool]->[Bool]" 5 | diff -rud bench/tiers-txt/$*-Bools-Bools.txt -
	$< "[Nat]->[Nat]"   6 | diff -rud bench/tiers-txt/$*-Nats-Nats.txt   -
	$< "[Int]->[Int]"   6 | diff -rud bench/tiers-txt/$*-Ints-Ints.txt   -
	# more functions
	$< "Nat2->Nat2"       | diff -rud bench/tiers-txt/$*-Nat2-Nat2.txt   -
	$< "Nat2->Nat3"       | diff -rud bench/tiers-txt/$*-Nat2-Nat3.txt   -
	$< "Nat3->Nat2"       | diff -rud bench/tiers-txt/$*-Nat3-Nat2.txt   -
	$< "Nat3->Nat3"       | diff -rud bench/tiers-txt/$*-Nat3-Nat3.txt   -

bench/tiers-%.txt: bench/tiers-%
	# functions
	$< "()->()"           > bench/tiers-txt/$*-U-U.txt
	$< "Bool->Bool"       > bench/tiers-txt/$*-Bool-Bool.txt
	$< "Bool->Bool->Bool" > bench/tiers-txt/$*-Bool-Bool-Bool.txt
	$< "Bool->()"         > bench/tiers-txt/$*-Bool-U.txt
	$< "()->Bool"         > bench/tiers-txt/$*-U-Bool.txt
	$< "Int->Int"       9 > bench/tiers-txt/$*-Int-Int.txt
	$< "Nat->Nat"       9 > bench/tiers-txt/$*-Nat-Nat.txt
	$< "Nat->()"        6 > bench/tiers-txt/$*-Nat-U.txt
	$< "()->Nat"        6 > bench/tiers-txt/$*-U-Nat.txt
	$< "Int->Int->Int"  6 > bench/tiers-txt/$*-Int-Int-Int.txt
	$< "Nat->Nat->Nat"  6 > bench/tiers-txt/$*-Nat-Nat-Nat.txt
	$< "(Nat,Nat)->Nat" 6 > bench/tiers-txt/$*-Nat,Nat-Nat.txt
	$< "Maybe Bool->Bool" > bench/tiers-txt/$*-MBool-Bool.txt
	$< "Bool->Maybe Bool" > bench/tiers-txt/$*-Bool-MBool.txt
	$< "Maybe Bool->Maybe Bool" > bench/tiers-txt/$*-MBool-MBool.txt
	# functions with mixed arguments
	$< "Bool->Int->Bool" 6 > bench/tiers-txt/$*-Bool-Int-Bool.txt
	$< "Int->Bool->Bool" 6 > bench/tiers-txt/$*-Int-Bool-Bool.txt
	# functions with 3 arguments
	$< "Int->Int->Int->Int"     4 > bench/tiers-txt/$*-Int-Int-Int-Int.txt
	$< "Bool->Bool->Bool->Bool"   > bench/tiers-txt/$*-Bool-Bool-Bool-Bool.txt
	# functions of lists
	$< "[()]->[()]"     6 > bench/tiers-txt/$*-Us-Us.txt
	$< "[Bool]->[Bool]" 5 > bench/tiers-txt/$*-Bools-Bools.txt
	$< "[Nat]->[Nat]"   6 > bench/tiers-txt/$*-Nats-Nats.txt
	$< "[Int]->[Int]"   6 > bench/tiers-txt/$*-Ints-Ints.txt
	# more functions
	$< "Nat2->Nat2"       > bench/tiers-txt/$*-Nat2-Nat2.txt
	$< "Nat2->Nat3"       > bench/tiers-txt/$*-Nat2-Nat3.txt
	$< "Nat3->Nat2"       > bench/tiers-txt/$*-Nat3-Nat2.txt
	$< "Nat3->Nat3"       > bench/tiers-txt/$*-Nat3-Nat3.txt
