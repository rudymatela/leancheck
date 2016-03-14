# Makefile for LeanCheck
TESTS = tests/test           \
        tests/test-utils     \
        tests/test-derive    \
        tests/test-error     \
        tests/test-most      \
        tests/test-operators \
        tests/test-funshow   \
        tests/test-types
ALLOBJS = $(shell find Test -name \*.hs | sed -e 's/.hs$$/.o/')
OBJS = Test/Check.o \
       Test/Most.o \
       Test/Check/Function.o \
       Test/Check/Function/Show.o \
       Test/Check/Error.o
GHCIMPORTDIRS = .
# -dynamic is needed only for Test/Check/Derive.hs and tests/test-derive.hs
GHCFLAGS = -dynamic

all: $(OBJS)

all-all: $(ALLOBJS)

test: all-all $(TESTS)
	./tests/test
	./tests/test-utils
	./tests/test-derive
	./tests/test-error
	./tests/test-most
	./tests/test-operators
	./tests/test-funshow
	./tests/test-types

clean: clean-hi-o
	rm -f $(TESTS)

legacy-test: # needs ghc-7.8, ghc-7.6 and ghc-7.4 installed as such
	make clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.6 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make test GHC=ghc-7.4 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-7.8 test
	cabal clean && cabal-ghc-7.6 test
	cabal clean && cabal-ghc-7.4 test
	cabal clean

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

include mk/haskell.mk
