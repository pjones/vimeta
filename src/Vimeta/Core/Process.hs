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
-- | Utility functions for running external commands.
module Vimeta.Core.Process
       ( tagFile
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.Process

--------------------------------------------------------------------------------
-- Local imports:
import Vimeta.Core.Config
import Vimeta.Core.Vimeta

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Run the tagging command unless dry-run mode is in effect.
tagFile :: Text -> Vimeta IO ()
tagFile cmd = do
  dryRun <- configDryRun <$> asks ctxConfig
  if dryRun then doDryRun else doRealRun

  where
    doDryRun :: Vimeta IO ()
    doDryRun = verbose "dry run: skipping tagging command" >>
               verbose cmd

    doRealRun :: Vimeta IO ()
    doRealRun = do
      verbose cmd
      let cmd' = Text.unpack cmd
      code <- liftIO (spawnCommand cmd' >>= waitForProcess)

      case code of
        ExitSuccess   -> return ()
        ExitFailure n -> die ("command failed (" ++ show n ++ "): " ++ cmd')

