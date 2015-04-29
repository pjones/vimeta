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
module Vimeta.Process
       ( tagFile
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.Text as Text
import System.Exit
import System.Process
import Vimeta.Config
import Vimeta.Context

--------------------------------------------------------------------------------
-- | Run the tagging command unless dry-run mode is in effect.
tagFile :: String -> Vimeta IO ()
tagFile cmd = do
  dryRun <- configDryRun <$> asks ctxConfig
  if dryRun then doDryRun else doRealRun

  where
    doDryRun :: Vimeta IO ()
    doDryRun = verbose "dry run: skipping tagging command" >>
               verbose (Text.pack cmd)

    doRealRun :: Vimeta IO ()
    doRealRun = do code <- liftIO (spawnCommand cmd >>= waitForProcess)
                   case code of
                     ExitSuccess   -> return ()
                     ExitFailure _ -> die ("command failed: " ++ cmd)
