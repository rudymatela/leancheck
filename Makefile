# Makefile for llcheck
TESTS = tests/test tests/test-operators tests/test-types tests/test-utils tests/test-most tests/test-derive
OBJS = $(shell find Test -name \*.hs | sed -e 's/.hs$$/.o/')
GHCIMPORTDIRS = .
# -dynamic is needed only for Test/Check/Derive.hs and tests/test-derive.hs
GHCFLAGS = -dynamic

all: $(OBJS)

test: all $(TESTS)
	./tests/test
	./tests/test-operators
	./tests/test-types
	./tests/test-utils
	./tests/test-most
	./tests/test-derive

clean: clean-hi-o
	rm -f $(TESTS)

legacy-test: # needs ghc-7.8, ghc-7.6 and ghc-7.4 installed as such
	make clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.6 GHCFLAGS=-Werror
	make clean && make test GHC=ghc-7.4 GHCFLAGS=-Werror
	make clean

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-7.8 test
	cabal clean && cabal-ghc-7.6 test
	cabal clean && cabal-ghc-7.4 test
	cabal clean

include mk/haskell.mk
