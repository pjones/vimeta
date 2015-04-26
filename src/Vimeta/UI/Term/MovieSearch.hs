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
module Vimeta.UI.Term.MovieSearch
       ( search
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (formatTime)
import Network.API.TheMovieDB
import System.Console.Byline
import System.Locale (defaultTimeLocale)
import Vimeta.Config
import qualified Vimeta.Context as V
import Vimeta.Context hiding (ask)

--------------------------------------------------------------------------------
search :: Text -> Vimeta Movie
search initial = do
  context <- V.ask
  result  <- liftIO $ runByline (searchWithByline initial context)

  case result of
    Left e  -> die e
    Right m -> return m

--------------------------------------------------------------------------------
searchWithByline :: Text -> Context -> Byline IO (Either String Movie)
searchWithByline initial context = do
  name   <- fromMaybe initial <$> ask "search: " (Just initial)
  movies <- liftIO $ runTheMovieDBWithManager manager key (searchMovies name)

  case movies of
    Left e  -> return (Left $ show e)
    Right m -> movieMenu m

  where manager = ctxManager context
        key     = configTMDBKey (ctxConfig context)

--------------------------------------------------------------------------------
movieMenu :: [Movie] -> Byline IO (Either String Movie)
movieMenu movies = do
  answer <- askWithMenuRepeatedly mkMenu prompt eprompt

  return $ case answer of
    Match movie -> Right movie
    _           -> Left "you need to pick a valid movie"

  where
    -- The Menu.
    mkMenu = banner "Choose a movie: " (menu movies displayMovie)

    -- Menu prompt.
    prompt = "movie> "

    -- Prompt when someone fails to pick a movie.
    eprompt = "please choose a valid movie" <> fg red

    -- Menu item display for a movie.
    displayMovie m = mconcat [ text (movieTitle m)
                             , " ("
                             , text (year $ movieReleaseDate m)
                             , ")"
                             ]

    -- The movie's release date as a year.
    year Nothing  = "----"
    year (Just d) = Text.pack (formatTime defaultTimeLocale "%Y" d)
