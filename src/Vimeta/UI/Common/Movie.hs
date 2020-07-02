-- |
--
-- Copyright:
--   This file is part of the package vimeta. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/vimeta
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Vimeta.UI.Common.Movie
  ( tagMovie,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Network.API.TheMovieDB
import Vimeta.Core

-- | Run the tagger for the given file/movie combo.
tagMovie :: (MonadIO m) => FilePath -> Movie -> Vimeta m ()
tagMovie filename movie = do
  context <- ask

  let format = configFormatMovie (ctxConfig context)
      tmdbCfg = ctxTMDBCfg context

  withArtwork (moviePosterURLs tmdbCfg movie) $ \artwork ->
    case fromFormatString (formatMap artwork) "config.cmd_movie" format of
      Left e -> throwError e
      Right cmd -> tagFile cmd
  where
    formatMap :: Maybe FilePath -> FormatTable
    formatMap artwork =
      Map.fromList
        [ ('Y', formatFullDate $ movieReleaseDate movie),
          ('a', toText <$> artwork),
          ('d', Just (Text.take 255 $ movieOverview movie)),
          ('g', genreName <$> listToMaybe (movieGenres movie)),
          ('t', Just $ movieTitle movie),
          ('y', formatYear $ movieReleaseDate movie),
          ('f', Just $ toText filename)
        ]
