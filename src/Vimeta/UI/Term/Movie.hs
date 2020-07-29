{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

-- | Search for a movie and interact with the user through the terminal.
module Vimeta.UI.Term.Movie
  ( movieSearch,
  )
where

import Byline.Menu
import Network.API.TheMovieDB
import Vimeta.Core
import Vimeta.UI.Common.Util
import Vimeta.UI.Term.Common

-- | A wrapper around a movie.
newtype MovieItem = MovieItem Movie

instance ToStylizedText MovieItem where
  toStylizedText (MovieItem m) =
    mconcat
      [ text (movieTitle m),
        text (parens $ dayAsYear $ movieReleaseDate m)
      ]

-- | Search for a movie and interact with the user through the terminal.
movieSearch :: MonadIO m => Text -> Vimeta m Movie
movieSearch initial = do
  name <- askUntil searchPrompt (Just initial) (return . notBlank searchErr)
  movies <-
    tmdb (searchMovies name)
      >>= ( nonEmpty >>> \case
              Nothing -> throwError ("no matches for: " <> toString name)
              Just ms -> pure (MovieItem <$> ms)
          )
  MovieItem movie <- askWithMenuRepeatedly (mkMenu movies) prompt eprompt
  logID movie >> tmdb (fetchMovie (movieID movie))
  where
    -- The Menu.
    mkMenu movies = menuBanner (text "Choose a movie:") (menu movies)
    -- Search prompt.
    searchPrompt = text "search (movie name): "
    -- Search error text.
    searchErr = "please enter a valid search term" <> fg red
    -- Menu prompt.
    prompt = text "Which is the correct movie? "
    -- Prompt when someone fails to pick a movie.
    eprompt = "please choose a valid movie" <> fg red
    -- Log a movie ID.
    logID movie =
      verbose $
        "using movie ID: "
          <> show (movieID movie)
