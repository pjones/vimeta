{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

-- | Search for a TV series by interacting with the user through the
-- terminal.
module Vimeta.UI.Term.TV
  ( tvSearch,
  )
where

import Byline.Menu
import Network.API.TheMovieDB
import Vimeta.Core
import Vimeta.UI.Common.Util
import Vimeta.UI.Term.Common

newtype TVItem = TVItem TV

instance ToStylizedText TVItem where
  toStylizedText (TVItem series) =
    mconcat
      [ text (tvName series),
        text (parens $ dayRange (tvFirstAirDate series) (tvLastAirDate series))
      ]

tvSearch :: MonadIO m => Vimeta m TV
tvSearch = do
  let prompt = text "search (series name): "
      mprompt = text "Which is the correct TV series? "
      sprompt = "a search term is required" <> fg red
      eprompt = "please choose a TV series" <> fg red
      mkMenu = menuBanner (text "Choose a TV series:") . menu
      logID tv = verbose $ "using TV ID: " <> show (tvID tv)

  name <- askUntil prompt Nothing (return . notBlank sprompt)
  series <-
    tmdb (searchTV name)
      >>= ( nonEmpty >>> \case
              Nothing -> throwError ("no matches for " <> toString name)
              Just xs -> pure (TVItem <$> xs)
          )
  TVItem tv <- askWithMenuRepeatedly (mkMenu series) mprompt eprompt
  logID tv >> tmdb (fetchFullTVSeries (tvID tv))
