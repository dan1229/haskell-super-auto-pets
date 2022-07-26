
# Typically, `cabal` should be called with the builddir set
CABAL = cabal --builddir=$(builddir)


build:
	$(CABAL) build

run:
	stack run

test:
	$(CABAL) test