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

diff-test: diff-test-tiers

%.test: %
	./$<

clean: clean-hi-o clean-haddock
	rm -f $(TESTS)

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
	# functions
	./bench/tiers "()->()"           | diff -rud tests/diff/tiers-U-U         -
	./bench/tiers "Bool->Bool"       | diff -rud tests/diff/tiers-Bool-Bool   -
	./bench/tiers "Bool->()"         | diff -rud tests/diff/tiers-Bool-U      -
	./bench/tiers "()->Bool"         | diff -rud tests/diff/tiers-U-Bool      -
	./bench/tiers "Int->Int"       6 | diff -rud tests/diff/tiers-Int-Int     -
	./bench/tiers "Nat->Nat"       6 | diff -rud tests/diff/tiers-Nat-Nat     -
	./bench/tiers "()->Nat"        6 | diff -rud tests/diff/tiers-U-Nat       -
	./bench/tiers "Nat->()"        6 | diff -rud tests/diff/tiers-Nat-U       -
	./bench/tiers "Int->Int->Int"  4 | diff -rud tests/diff/tiers-Int-Int-Int -
	./bench/tiers "Nat->Nat->Nat"  4 | diff -rud tests/diff/tiers-Nat-Nat-Nat -
	./bench/tiers "(Nat,Nat)->Nat" 4 | diff -rud tests/diff/tiers-Nat,Nat-Nat -
	./bench/tiers "Maybe Bool->Bool" | diff -rud tests/diff/tiers-MBool-Bool  -
	./bench/tiers "Bool->Maybe Bool" | diff -rud tests/diff/tiers-Bool-MBool  -
	./bench/tiers "Maybe Bool->Maybe Bool" | diff -rud tests/diff/tiers-MBool-MBool -
	# more functions
	./bench/tiers "Nat2->Nat2"       | diff -rud tests/diff/tiers-Nat2-Nat2   -
	./bench/tiers "Nat2->Nat3"       | diff -rud tests/diff/tiers-Nat2-Nat3   -
	./bench/tiers "Nat3->Nat2"       | diff -rud tests/diff/tiers-Nat3-Nat2   -
	./bench/tiers "Nat3->Nat3"       | diff -rud tests/diff/tiers-Nat3-Nat3   -
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
	# functions
	./bench/tiers "()->()"           > tests/diff/tiers-U-U
	./bench/tiers "Bool->Bool"       > tests/diff/tiers-Bool-Bool
	./bench/tiers "Bool->()"         > tests/diff/tiers-Bool-U
	./bench/tiers "()->Bool"         > tests/diff/tiers-U-Bool
	./bench/tiers "Int->Int"       6 > tests/diff/tiers-Int-Int
	./bench/tiers "Nat->Nat"       6 > tests/diff/tiers-Nat-Nat
	./bench/tiers "Nat->()"        6 > tests/diff/tiers-Nat-U
	./bench/tiers "()->Nat"        6 > tests/diff/tiers-U-Nat
	./bench/tiers "Int->Int->Int"  4 > tests/diff/tiers-Int-Int-Int
	./bench/tiers "Nat->Nat->Nat"  4 > tests/diff/tiers-Nat-Nat-Nat
	./bench/tiers "(Nat,Nat)->Nat" 4 > tests/diff/tiers-Nat,Nat-Nat
	./bench/tiers "Maybe Bool->Bool" > tests/diff/tiers-MBool-Bool
	./bench/tiers "Bool->Maybe Bool" > tests/diff/tiers-Bool-MBool
	./bench/tiers "Maybe Bool->Maybe Bool" > tests/diff/tiers-MBool-MBool
	# more functions
	./bench/tiers "Nat2->Nat2"       > tests/diff/tiers-Nat2-Nat2
	./bench/tiers "Nat2->Nat3"       > tests/diff/tiers-Nat2-Nat3
	./bench/tiers "Nat3->Nat2"       > tests/diff/tiers-Nat3-Nat2
	./bench/tiers "Nat3->Nat3"       > tests/diff/tiers-Nat3-Nat3
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
