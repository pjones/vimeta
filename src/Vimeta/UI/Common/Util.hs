-- |
--
-- Copyright:
--   This file is part of the package vimeta. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/vimeta
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Vimeta.UI.Common.Util
  ( parens,
    dayAsYear,
    dayRange,
  )
where

import Data.Time (Day, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)

-- | Wrap some text with parenthesis.
parens :: Text -> Text
parens t = " (" <> t <> ")"

-- | Format a 'Maybe Day' as a year ('Text').
dayAsYear :: Maybe Day -> Text
dayAsYear Nothing = "----"
dayAsYear (Just d) = toText (formatTime defaultTimeLocale "%Y" d)

-- | Given a start 'Day' and an end 'Day', produce a string
-- representing a range.
dayRange :: Maybe Day -> Maybe Day -> Text
dayRange d1 d2 = dayAsYear d1 <> " - " <> dayAsYear d2
