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
module Vimeta.UI.CommandLine.Movie
  ( Options,
    optionsParser,
    run,
  )
where

import Network.API.TheMovieDB
import Options.Applicative
import System.FilePath
import Vimeta.Core
import Vimeta.UI.CommandLine.Common
import Vimeta.UI.Common.Movie
import Vimeta.UI.Term.Movie

data Options = Options
  { optsMovieID :: Maybe ItemID,
    optsFile :: FilePath,
    optsCommon :: CommonOptions
  }

optionsParser :: Parser Options
optionsParser =
  Options <$> optional (option auto getMovieID)
    <*> argument str (metavar "FILE")
    <*> commonOptions
  where
    -- Parser options for @optsMovieID@.
    getMovieID =
      mconcat
        [ short 'i',
          long "id",
          metavar "ID",
          help "Movie ID assigned by TheMovieDB.org"
        ]

run :: Options -> IO (Either String ())
run opts = execVimeta (updateConfig $ optsCommon opts) $
  case optsMovieID opts of
    Just mid -> do
      movie <- tmdb (fetchMovie mid)
      tagMovie (optsFile opts) movie
    Nothing -> do
      movie <- movieSearch initialTitle
      tagMovie (optsFile opts) movie
  where
    -- Calculate an initial search title from the file name.
    initialTitle :: Text
    initialTitle = toText $ dropExtension (takeFileName $ optsFile opts)
