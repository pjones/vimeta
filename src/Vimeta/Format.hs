{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.Format (format) where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
format :: Map Char Text         -- ^ Format character mapping.
       -> Text                  -- ^ Input text.
       -> Text                  -- ^ Output text.
format map input = Text.concat $ replaceAll $ Text.splitOn "%" input
  where
    replaceAll :: [Text] -> [Text]
    replaceAll []             = []
    replaceAll (left:matches) = left ; map replaceOne matches

    replaceOne :: Text -> Text
    replaceOne raw = case Map.lookup (Text.take 1 raw) map of
                       Nothing  -> raw
                       Just new -> new <> Text.drop 1 raw
