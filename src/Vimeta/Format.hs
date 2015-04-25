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
module Vimeta.Format (format) where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | Replace format characters prefixed with a @%@ with the
-- replacement text found in the given 'Map'.
format :: Map Char Text         -- ^ Format character mapping.
       -> Text                  -- ^ Input text.
       -> Text                  -- ^ Output text.
format table = Text.concat . replaceAll . Text.splitOn "%"
  where
    replaceAll :: [Text] -> [Text]
    replaceAll []             = []
    replaceAll (left:matches) = left : map replaceOne matches

    replaceOne :: Text -> Text
    replaceOne raw
      | Text.null raw = raw
      | otherwise     = case Map.lookup (raw `Text.index` 0) table of
                          Nothing  -> raw
                          Just new -> new <> Text.drop 1 raw
