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
module Vimeta.UI.Term.TVSearch
       ( search
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import Network.API.TheMovieDB
import System.Console.Byline
import Vimeta.Context hiding (ask)
import Vimeta.UI.Term.Common
import Vimeta.UI.Util

--------------------------------------------------------------------------------
search :: Vimeta (Byline IO) TV
search = do
  let prompt  = "search (series name): "
      message = "a search term is required"
      mprompt = "tv> "
      eprompt = text message <> fg red

  name <- bylineMaybe message $ ask prompt Nothing
  series <- tmdb (searchTV name)
  answer <- byline $ askWithMenuRepeatedly (mkMenu series) mprompt eprompt

  case answer of
    Match tv -> tmdb $ fetchFullTVSeries (tvID tv)
    _        -> die "you need to pick a valid TV series"


  where
    mkMenu series = banner "Choose a TV series:" (menu series displayTV)

    displayTV series =
      mconcat [ text (tvName series)
              , text (parens $ dayRange (tvFirstAirDate series) (tvLastAirDate series))
              ]
