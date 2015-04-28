{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.Process
       ( tagFile
       ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
tagFile :: Text -> IO (Either String ())
tagFile t = fmap Right (putStrLn $ Text.unpack t)
