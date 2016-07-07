# Makefile for LeanCheck
TESTS = tests/test           \
        tests/test-derive    \
        tests/test-error     \
        tests/test-fun       \
        tests/test-funshow   \
        tests/test-io        \
        tests/test-operators \
        tests/test-tiers     \
        tests/test-types
LISTHS   = find src -name \*.hs
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

test: all-all $(TESTS)
	./tests/test
	./tests/test-derive
	./tests/test-error
	./tests/test-fun
	./tests/test-funshow
	./tests/test-io
	./tests/test-operators
	./tests/test-tiers
	./tests/test-types

clean: clean-hi-o clean-haddock
	rm -f $(TESTS)

ghci: src/Test/LeanCheck.ghci

install:
	@echo "use \`cabal install' instead"

list-hs:
	$(LISTHS)

list-objs:
	$(LISTOBJS)

legacy-test: # needs ghc-7.8, ghc-7.6 and ghc-7.4 installed as such
	make clean && make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.8 GHCFLAGS="-Werror -dynamic"
	make clean && make test GHC=ghc-7.6 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean && make test GHC=ghc-7.4 GHCFLAGS="-Werror -fno-warn-unrecognised-pragmas"
	make clean

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-7.10 test
	cabal clean && cabal-ghc-7.8 test
	cabal clean && cabal-ghc-7.6 test
	cabal clean && cabal-ghc-7.4 test
	cabal clean

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

doc/index.html: $(ALLHS)
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(ALLHS) --no-print-missing-docs --title=leancheck
	@echo 'NOTE: please ensure that there are *only* 7'
	@echo '      undocumented functions on Test.LeanCheck'
	@echo '      as "OPTIONS_HADDOCK prune" is active'
	@echo '      to hide cons6...cons12'

include mk/haskell.mk
