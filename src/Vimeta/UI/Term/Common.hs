{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.UI.Term.Common
  ( notBlank,
  )
where

import Byline
import qualified Data.Text as Text

-- | Check the input text to see if it is blank.  If it is, return the
-- given error message in 'Left'.
notBlank :: Stylized Text -> Text -> Either (Stylized Text) Text
notBlank errortxt input =
  let clean = Text.strip input
   in if Text.null clean
        then Left errortxt
        else Right clean
