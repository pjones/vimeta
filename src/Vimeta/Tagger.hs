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
module Vimeta.Tagger
       ( Tagger (..)
       , formatStringsForTagger
       ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
data Tagger = AtomicParsley

--------------------------------------------------------------------------------
formatStringsForTagger :: Tagger -> (Text, Text)
formatStringsForTagger AtomicParsley = (apMovie, apTV)

--------------------------------------------------------------------------------
-- | Common strings for AtomicParsley.
apPrefix, apSuffix :: Text
apPrefix = "AtomicParsley"
apSuffix = "--overWrite"

--------------------------------------------------------------------------------
-- | Format string for movies.
apMovie :: Text
apMovie = Text.intercalate " "
          [ apPrefix
          , "%f"
          , "--stik value=9"
          , "%{--year %Y%}"
          , "--title %t"
          , "--description %d"
          , "%{--genre %G%}"
          , "--artwork REMOVE_ALL %{--artwork %a%}"
          , apSuffix
          ]

--------------------------------------------------------------------------------
-- | Format string for TV episodes.
apTV :: Text
apTV = Text.intercalate " "
       [ apPrefix
       , "%f"
       , "--stik 'TV Show'"
       , "%{--year %Y%}"
       , "--title %t"
       , "--description %d"
       , "--TVShowName %n"
       , "--TVSeasonNum %s"
       , "--TVEpisodeNum %e"
       , "--tracknum %e"
       , "--artwork REMOVE_ALL %{--artwork %a%}"
       , apSuffix
       ]
