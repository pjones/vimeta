{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.UI.CommandLine (run) where

--------------------------------------------------------------------------------
import Data.Monoid
import Options.Applicative
import System.Exit

--------------------------------------------------------------------------------
import qualified Vimeta.UI.CommandLine.Config as Config
import qualified Vimeta.UI.CommandLine.Movie  as Movie
import qualified Vimeta.UI.CommandLine.TV     as TV

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
data Command = CmdConfig Config.Options
             | CmdMovie  Movie.Options
             | CmdTV     TV.Options

--------------------------------------------------------------------------------
optionsParser :: Parser Command
optionsParser = subparser $ mconcat [config, movie, tv]
  where
    config =
      command "config"
      (info (CmdConfig <$> Config.optionsParser) (progDesc configDesc))

    movie =
      command "movie"
      (info (CmdMovie <$> Movie.optionsParser) (progDesc movieDesc))

    tv =
      command "tv"
      (info (CmdTV <$> TV.optionsParser) (progDesc tvDesc))

    configDesc =
      "Create a new configuration file"

    movieDesc =
      "Tag a movie file using data from TheMovieDB.org"

    tvDesc =
      "Tag episode files using data from TheMovieDB.org"

--------------------------------------------------------------------------------
run :: IO ()
run = do
  options <- execParser $ info (optionsParser <**> helper) idm

  case options of
    CmdConfig o -> Config.run o
    CmdMovie  o -> Movie.run  o
    CmdTV     o -> TV.run     o

  exitSuccess
