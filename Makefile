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

test: $(patsubst %,%.test,$(TESTS)) diff-test-tiers

%.test: %
	./$<

clean: clean-hi-o clean-haddock
	rm -f $(TESTS)

ghci: src/Test/LeanCheck.ghci

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
	./bench/tiers "()"              | diff -rud tests/diff/tiers             -
	./bench/tiers "Int"             | diff -rud tests/diff/tiers-Int         -
	./bench/tiers "Nat"             | diff -rud tests/diff/tiers-Nat         -
	./bench/tiers "Integer"         | diff -rud tests/diff/tiers-Integer     -
	./bench/tiers "Bool"            | diff -rud tests/diff/tiers-Bool        -
	./bench/tiers "Char"            | diff -rud tests/diff/tiers-Char        -
	# lists
	./bench/tiers "[()]"            | diff -rud tests/diff/tiers-Us          -
	./bench/tiers "[Int]"         6 | diff -rud tests/diff/tiers-Ints        -
	./bench/tiers "[Nat]"         6 | diff -rud tests/diff/tiers-Nats        -
	./bench/tiers "[Bool]"        6 | diff -rud tests/diff/tiers-Bools       -
	./bench/tiers "String"        6 | diff -rud tests/diff/tiers-String      -
	# pairs
	./bench/tiers "(Int,Int)"       | diff -rud tests/diff/tiers-Int,Int     -
	./bench/tiers "(Nat,Nat)"       | diff -rud tests/diff/tiers-Nat,Nat     -
	./bench/tiers "(Int,Int,Int)" 6 | diff -rud tests/diff/tiers-Int,Int,Int -
	./bench/tiers "(Nat,Nat,Nat)" 6 | diff -rud tests/diff/tiers-Nat,Nat,Nat -
	# lists & pairs
	./bench/tiers "[((),())]"       | diff -rud tests/diff/tiers-U,Us        -
	./bench/tiers "([()],[()])"     | diff -rud tests/diff/tiers-Us,Us       -
	# functions
	./bench/tiers "Int->Int"      6 | diff -rud tests/diff/tiers-Int-Int     -
	./bench/tiers "Nat->Nat"      6 | diff -rud tests/diff/tiers-Nat-Nat     -

update-diff-test-tiers: bench/tiers
	# simple types
	./bench/tiers "()"              > tests/diff/tiers
	./bench/tiers "Int"             > tests/diff/tiers-Int
	./bench/tiers "Nat"             > tests/diff/tiers-Nat
	./bench/tiers "Integer"         > tests/diff/tiers-Integer
	./bench/tiers "Bool"            > tests/diff/tiers-Bool
	./bench/tiers "Char"            > tests/diff/tiers-Char
	# lists
	./bench/tiers "[()]"            > tests/diff/tiers-Us
	./bench/tiers "[Int]"         6 > tests/diff/tiers-Ints
	./bench/tiers "[Nat]"         6 > tests/diff/tiers-Nats
	./bench/tiers "[Bool]"        6 > tests/diff/tiers-Bools
	./bench/tiers "String"        6 > tests/diff/tiers-String
	# pairs
	./bench/tiers "(Int,Int)"       > tests/diff/tiers-Int,Int
	./bench/tiers "(Nat,Nat)"       > tests/diff/tiers-Nat,Nat
	./bench/tiers "(Int,Int,Int)" 6 > tests/diff/tiers-Int,Int,Int
	./bench/tiers "(Nat,Nat,Nat)" 6 > tests/diff/tiers-Nat,Nat,Nat
	# lists & pairs
	./bench/tiers "[((),())]"       > tests/diff/tiers-U,Us
	./bench/tiers "([()],[()])"     > tests/diff/tiers-Us,Us
	# functions
	./bench/tiers "Int->Int"      6 > tests/diff/tiers-Int-Int
	./bench/tiers "Nat->Nat"      6 > tests/diff/tiers-Nat-Nat
