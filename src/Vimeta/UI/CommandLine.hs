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
import Data.Version (showVersion)
import Options.Applicative
import System.Exit

--------------------------------------------------------------------------------
import qualified Vimeta.UI.CommandLine.Config as Config
import qualified Vimeta.UI.CommandLine.Movie  as Movie
import qualified Vimeta.UI.CommandLine.TV     as TV

--------------------------------------------------------------------------------
-- Cabal generated source files:
import Paths_vimeta (version)

--------------------------------------------------------------------------------
data Command = CmdVersion
             | CmdConfig Config.Options
             | CmdMovie  Movie.Options
             | CmdTV     TV.Options

--------------------------------------------------------------------------------
optionsParser :: Parser Command
optionsParser = verbose <|> commands
  where
    verbose =
      flag' CmdVersion
      (long "version" <> help "Print version and exit")

    commands =
      subparser $ mconcat [config, movie, tv]

    subcommand name desc parser =
      command name (info (parser <**> helper) (progDesc desc))

    config =
      subcommand "config" configDesc (CmdConfig <$> Config.optionsParser)

    movie =
      subcommand "movie" movieDesc (CmdMovie <$> Movie.optionsParser)

    tv =
      subcommand "tv" tvDesc (CmdTV <$> TV.optionsParser)

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
    CmdVersion  -> putStrLn (showVersion version)
    CmdConfig o -> Config.run o
    CmdMovie  o -> Movie.run  o
    CmdTV     o -> TV.run     o

  exitSuccess
