################################################################################
.PHONEY: all install restart

################################################################################
# Set up the default target.
all::

################################################################################
# Ask `git' to update the submodule and make haskell.mk available.
util/haskell.mk:
	git submodule update --init

################################################################################
# From util/haskell.mk (git submodule update --init)
CABAL_FLAGS         =
CABAL_DEP_PROFILING = -O2
CABAL_ADD_SOURCE    = vendor/themoviedb
CABAL_ADD_SOURCE   += vendor/thetvdb

################################################################################
include util/haskell.mk
