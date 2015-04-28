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
module Vimeta.UI.CommandLine.Movie
       ( Options
       , optionsParser
       , run
       ) where

--------------------------------------------------------------------------------
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Network.API.TheMovieDB
import Options.Applicative
import System.FilePath
import Vimeta.Config
import Vimeta.Context
import Vimeta.Download
import Vimeta.Format
import Vimeta.Process
import qualified Vimeta.UI.Term.MovieSearch as MovieSearch

--------------------------------------------------------------------------------
data Options = Options
  { optsMovieID :: Maybe ItemID
  , optsFile    :: FilePath
  }

--------------------------------------------------------------------------------
optionsParser :: Parser Options
optionsParser = Options <$> optional (option auto getMovieID)
                        <*> argument str (metavar "FILE")

  where
    -- Parser options for @optsMovieID@.
    getMovieID = mconcat [ short 'i'
                         , long "id"
                         , metavar "ID"
                         , help "Movie ID assigned by TheMovieDB.org"
                         ]

--------------------------------------------------------------------------------
run :: Options -> IO ()
run opts = runVimetaBylineApp $
  case optsMovieID opts of
    Just mid -> do
      movie <- tmdb (fetchMovie mid)
      tagMovie (optsFile opts) movie

    Nothing -> do
      movie <- MovieSearch.search initialTitle
      tagMovie (optsFile opts) movie

  where
    -- Calculate an initial search title from the file name.
    initialTitle :: Text
    initialTitle = Text.pack $ dropExtension (takeFileName $ optsFile opts)

--------------------------------------------------------------------------------
-- | Run the tagger for the given file/movie combo.
tagMovie :: (MonadIO m) => FilePath -> Movie -> Vimeta m ()
tagMovie filename movie = do
  context <- ask

  let format  = configFormatMovie (ctxConfig context)
      tmdbCfg = ctxTMDBCfg context

  result <- withArtwork (moviePosterURLs tmdbCfg movie) $ \artwork ->
    case fromFormatString (formatMap artwork) "config.cmd_movie" format of
      Left e    -> return (Left e)
      Right cmd -> tagFile cmd

  case result of
    Left e -> die e
    _      -> return ()

  where
    formatMap :: Maybe FilePath -> FormatTable
    formatMap artwork = Map.fromList
      [ ('Y', formatFullDate $ movieReleaseDate movie)
      , ('a', Text.pack <$> artwork)
      , ('d', Just $ movieOverview movie)
      , ('g', genreName <$> listToMaybe (movieGenres movie))
      , ('t', Just $ movieTitle movie)
      , ('y', formatYear $ movieReleaseDate movie)
      , ('f', Just $ Text.pack filename)
      ]
