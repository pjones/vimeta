{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.Movie where
import qualified Network.API.TheMovieDB as TMDB

data Config = Config
  { movieID   :: TMDB.MovieID
  , movieFile :: FilePath
  } deriving (Eq, Show)
