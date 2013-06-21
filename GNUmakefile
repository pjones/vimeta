################################################################################
.PHONEY: all clean test setup

################################################################################
all:
	cabal-dev install

################################################################################
clean:
	cabal-dev clean

################################################################################
test:
	cabal-dev install --enable-tests

################################################################################
setup:
	git submodule update --init
	- cabal-dev ghc-pkg unregister thetvdb
	cabal-dev add-source vendor/thetvdb
	- cabal-dev ghc-pkg unregister themoviedb
	cabal-dev add-source vendor/themoviedb
