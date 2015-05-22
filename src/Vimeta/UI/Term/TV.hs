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
-- | Search for a TV series by interacting with the user through the terminal.
module Vimeta.UI.Term.TV
       ( tvSearch
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import qualified Data.Text as Text
import Network.API.TheMovieDB
import System.Console.Byline
import Vimeta.Core hiding (ask)
import Vimeta.UI.Common.Util
import Vimeta.UI.Term.Common

--------------------------------------------------------------------------------
tvSearch :: Vimeta (Byline IO) TV
tvSearch = do
  let prompt  = "search (series name): "
      mprompt = "Which is the correct TV series? "
      sprompt = "a search term is required" <> fg red
      eprompt = "please choose a TV series" <> fg red

  name <- byline (askUntil prompt Nothing $ notEmpty sprompt)
  series <- tmdb (searchTV name)
  answer <- byline $ askWithMenuRepeatedly (mkMenu series) mprompt eprompt

  case answer of
    Match tv -> logID tv >> tmdb (fetchFullTVSeries (tvID tv))
    _        -> die "you need to pick a valid TV series"


  where
    -- The menu.
    mkMenu series = banner "Choose a TV series:" (menu series displayTV)

    -- Function to display possible matches.
    displayTV series =
      mconcat [ text (tvName series)
              , text (parens $ dayRange (tvFirstAirDate series) (tvLastAirDate series))
              ]

    -- Log the TV ID.
    logID tv = verbose $ "using TV ID: " <> Text.pack (show $ tvID tv)
