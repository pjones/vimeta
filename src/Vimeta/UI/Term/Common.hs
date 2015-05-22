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
module Vimeta.UI.Term.Common
       ( byline
       , notEmpty
       , execVimetaBylineApp
       ) where

--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Byline hiding (ask)
import System.Exit (exitSuccess, exitFailure)
import Vimeta.Core
import Vimeta.Core.Vimeta (Vimeta(..))

--------------------------------------------------------------------------------
-- | Run a 'Byline' operation.
byline :: Byline IO a -> Vimeta (Byline IO) a
byline = Vimeta . lift . lift

--------------------------------------------------------------------------------
notEmpty :: Stylized -> Text -> IO (Either Stylized Text)
notEmpty errortxt input = return $ if Text.length clean > 0
                                     then Right clean
                                     else Left errortxt
  where
    clean :: Text
    clean = Text.strip input

--------------------------------------------------------------------------------
-- | Helper function to run a 'Vimeta' value based in 'Byline'.
execVimetaBylineApp :: (Config -> Config) -> Vimeta (Byline IO) () -> IO ()
execVimetaBylineApp cf vimeta = void $ runByline $ do
    v <- execVimeta cf vimeta

    case v of
      Right _ -> liftIO exitSuccess
      Left  e -> reportLn Error (text $ Text.pack e) >> liftIO exitFailure
