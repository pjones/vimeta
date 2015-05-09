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
       , bylineMaybe
       , execVimetaBylineApp
       ) where

--------------------------------------------------------------------------------
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.Byline hiding (ask)
import System.Exit (exitSuccess, exitFailure)
import Vimeta.Config
import Vimeta.Context

--------------------------------------------------------------------------------
-- | Run a 'Byline' operation.
byline :: Byline IO a -> Vimeta (Byline IO) a
byline = Vimeta . lift . lift

--------------------------------------------------------------------------------
bylineMaybe :: Text -> Byline IO (Maybe a) -> Vimeta (Byline IO) a
bylineMaybe msg bm = do
  result <- byline bm

  case result of
    Nothing -> die (Text.unpack msg)
    Just a  -> return a

--------------------------------------------------------------------------------
-- | Helper function to run a 'Vimeta' value based in 'Byline'.
execVimetaBylineApp :: (Config -> Config) -> Vimeta (Byline IO) () -> IO ()
execVimetaBylineApp cf vimeta = runByline $ do
    v <- execVimeta cf vimeta

    case v of
      Right _ -> liftIO exitSuccess
      Left  e -> reportLn Error (text $ Text.pack e) >> liftIO exitFailure