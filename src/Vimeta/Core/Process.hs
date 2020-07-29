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
--
-- | Utility functions for running external commands.
module Vimeta.Core.Process
  ( tagFile,
  )
where

import System.Exit (ExitCode (..))
import System.Process
import Vimeta.Core.Config
import Vimeta.Core.Vimeta

-- | Run the tagging command unless dry-run mode is in effect.
tagFile :: Text -> Vimeta IO ()
tagFile cmd = do
  dryRun <- configDryRun <$> asks ctxConfig
  if dryRun then doDryRun else doRealRun
  where
    doDryRun :: Vimeta IO ()
    doDryRun =
      verbose "dry run: skipping tagging command"
        >> verbose cmd
    doRealRun :: Vimeta IO ()
    doRealRun = do
      verbose cmd
      code <- liftIO (spawnCommand (toString cmd) >>= waitForProcess)

      case code of
        ExitSuccess ->
          pass
        ExitFailure n ->
          throwError ("command failed (" ++ show n ++ "): " ++ toString cmd)
