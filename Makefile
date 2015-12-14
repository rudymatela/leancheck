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

legacy-test: # needs ghc-7.8 and ghc-7.6 installed as such
	make clean && make test GHC=ghc-7.8           && \
	make clean && make test GHC=ghc-7.6 GHCFLAGS= && \
	make clean

include mk/haskell.mk
