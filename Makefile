# Makefile for llcheck

OBJS = $(shell find Test -name \*.hs | sed -e 's/hs$$/o/')
INFS = $(shell find Test -name \*.hs | sed -e 's/hs$$/hi/')
TESTS = tests/test tests/test-operators tests/test-types tests/test-utils

%.o: %.hs
	ghc $<

%: %.hs
	ghc $<

all: $(OBJS) $(INFS)

test: all $(TESTS)
	./tests/test
	./tests/test-operators
	./tests/test-types
	./tests/test-utils

clean:
	rm -f $(OBJS) $(INFS) $(TESTS)
	rm -f tests/*.hi tests/*.o
