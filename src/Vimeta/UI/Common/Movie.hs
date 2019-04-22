{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.UI.Common.Movie
       ( tagMovie
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Network.API.TheMovieDB
import Vimeta.Core

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Run the tagger for the given file/movie combo.
tagMovie :: (MonadIO m) => FilePath -> Movie -> Vimeta m ()
tagMovie filename movie = do
  context <- ask

  let format  = configFormatMovie (ctxConfig context)
      tmdbCfg = ctxTMDBCfg context

  withArtwork (moviePosterURLs tmdbCfg movie) $ \artwork ->
    case fromFormatString (formatMap artwork) "config.cmd_movie" format of
      Left e    -> die e
      Right cmd -> tagFile cmd

  where
    formatMap :: Maybe FilePath -> FormatTable
    formatMap artwork = Map.fromList
      [ ('Y', formatFullDate $ movieReleaseDate movie)
      , ('a', Text.pack <$> artwork)
      , ('d', Just (Text.take 255 $ movieOverview movie))
      , ('g', genreName <$> listToMaybe (movieGenres movie))
      , ('t', Just $ movieTitle movie)
      , ('y', formatYear $ movieReleaseDate movie)
      , ('f', Just $ Text.pack filename)
      ]
