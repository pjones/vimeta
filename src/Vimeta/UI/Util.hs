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
module Vimeta.UI.Util
       ( parens
       , dayAsYear
       , dayRange
       ) where

--------------------------------------------------------------------------------
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, formatTime)
import System.Locale (defaultTimeLocale)

--------------------------------------------------------------------------------
-- | Wrap some text with parenthesis.
parens :: Text -> Text
parens t = "(" <> t <> ")"

--------------------------------------------------------------------------------
-- | Format a 'Maybe Day' as a year ('Text').
dayAsYear :: Maybe Day -> Text
dayAsYear Nothing  = "----"
dayAsYear (Just d) = Text.pack (formatTime defaultTimeLocale "%Y" d)

--------------------------------------------------------------------------------
-- | Given a start 'Day' and an end 'Day', produce a string
-- representing a range.
dayRange :: Maybe Day -> Maybe Day -> Text
dayRange d1 d2 = dayAsYear d1 <> " - " <> dayAsYear d2
