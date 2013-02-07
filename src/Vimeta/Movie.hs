{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.Movie (Config(..), update) where
import Data.Maybe (fromMaybe, listToMaybe)
import Safe (headDef)
import Vimeta.Download (download)
import qualified Network.API.TheMovieDB as TMDB
import qualified Network.API.TheMovieDB.Util as TMDBu
import qualified Vimeta.AtomicParsley as AP

-- The configuration information needed in order to update the
-- metadata in a movie file.
data Config = Config
  { movieID   :: TMDB.MovieID
  , movieFile :: FilePath
  } deriving (Eq, Show)

-- Returns a list of options that need to be passed to AtomicParsley.
atomicParsleyOptions :: TMDB.Movie -> Maybe FilePath -> AP.Options
atomicParsleyOptions mov poster =
  [ ("--stik",        "Movie")
  , ("--title",       TMDB.movieTitle mov)
  , ("--description", TMDB.movieOverview mov)
  , ("--genre",       TMDB.genreName $ headDef defGenere $ TMDB.movieGenres mov)
  , ("--artwork",     fromMaybe "REMOVE_ALL" poster)
  ]
  where defGenere = TMDB.Genre 0 "None"

-- Update the movie file with the given configuration.  FIXME: this
-- tries to use the largest poster image, but it needs to do a better
-- job.
update :: Config -> IO ()
update c = do ctx <- TMDBu.loadContext >>= maybe loadErr return
              cfg <- TMDB.config ctx
              mov <- TMDB.fetch ctx $ movieID c
              updateWithMetadata c mov $ imageURL cfg mov
  where loadErr = fail "failed to load TheMovieDB API key"
        imageURL cfg mov = listToMaybe $ reverse $ TMDB.moviePosterURLs cfg mov

-- Update the movie file with the given configuration and movie
-- metadata.
updateWithMetadata :: Config -> TMDB.Movie -> Maybe String -> IO ()
updateWithMetadata c m url = download url runAP
  where runAP poster = AP.update (movieFile c) $ atomicParsleyOptions m poster
