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
module Vimeta.UI.CommandLine.TV
  ( Options,
    optionsParser,
    run,
  )
where

import Network.API.TheMovieDB
import Options.Applicative
import Vimeta.Core
import Vimeta.UI.CommandLine.Common
import Vimeta.UI.Common.TV
import Vimeta.UI.Term.TV

data Options = Options
  { optsTVID :: Maybe ItemID,
    optsStartSeason :: Maybe Int,
    optsStartEpisode :: Maybe Int,
    optsMappingFile :: Maybe FilePath,
    optsFiles :: [FilePath],
    optsCommon :: CommonOptions
  }

optionsParser :: Parser Options
optionsParser =
  Options <$> optional (option auto infoTVID)
    <*> optional (option auto infoStartSeason)
    <*> optional (option auto infoStartEpisode)
    <*> optional (strOption infoMappingFile)
    <*> many (argument str (metavar "[FILE...]"))
    <*> commonOptions
  where
    infoTVID =
      short 'i' <> long "id" <> metavar "ID"
        <> help "Series ID assigned by TheMovieDB.org"
    infoStartSeason =
      short 's' <> long "season" <> metavar "NUM"
        <> help "Starting season number"
    infoStartEpisode =
      short 'e' <> long "episode" <> metavar "NUM"
        <> help "Starting episode number"
    infoMappingFile =
      short 'm' <> long "map" <> metavar "FILE"
        <> help "File to map files to seasons/episodes"

run :: Options -> IO (Either String ())
run opts = execVimeta (updateConfig $ optsCommon opts) $ do
  tv <- case optsTVID opts of
    Nothing -> tvSearch
    Just n -> tmdb (fetchFullTVSeries n)

  case optsMappingFile opts of
    Nothing -> fromFiles opts tv
    Just fn -> fromMappingFile opts tv fn

fromFiles :: (MonadIO m) => Options -> TV -> Vimeta m ()
fromFiles opts tv = case (optsStartSeason opts, optsStartEpisode opts) of
  (Just s, Nothing) -> tagWithFileOrder tv (EpisodeSpec s 1) (optsFiles opts)
  (Just s, Just e) -> tagWithFileOrder tv (EpisodeSpec s e) (optsFiles opts)
  (_, _) -> throwError "please use the --season option"

fromMappingFile :: (MonadIO m) => Options -> TV -> FilePath -> Vimeta m ()
fromMappingFile opts tv filename = do
  unless (null $ optsFiles opts) $
    throwError "don't give file arguments when using a mapping file"

  tagWithMappingFile tv filename
