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
-- | Search for a movie and interact with the user through the terminal.
module Vimeta.UI.Term.MovieSearch
       ( search
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.API.TheMovieDB
import System.Console.Byline
import Vimeta.Context hiding (ask)
import Vimeta.UI.Term.Common
import Vimeta.UI.Util

--------------------------------------------------------------------------------
-- | Search for a movie and interact with the user through the terminal.
search :: Text -> Vimeta (Byline IO) Movie
search initial = do
  name   <- byline $ fromMaybe initial <$> ask "search (movie name): " (Just initial)
  movies <- tmdb (searchMovies name)
  answer <- byline $ askWithMenuRepeatedly (mkMenu movies) prompt eprompt

  case answer of
    Match movie -> tmdb $ fetchMovie (movieID movie)
    _           -> die "you need to pick a valid movie"

  where
    -- The Menu.
    mkMenu movies = banner "Choose a movie:" (menu movies displayMovie)

    -- Menu prompt.
    prompt = "movie> "

    -- Prompt when someone fails to pick a movie.
    eprompt = "please choose a valid movie" <> fg red

    -- Menu item display for a movie.
    displayMovie m = mconcat [ text (movieTitle m)
                             , text (parens $ dayAsYear $ movieReleaseDate m)
                             ]
