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
-- | Common types/functions used in the command line interface.
module Vimeta.UI.CommandLine.Common
  ( CommonOptions,
    commonOptions,
    updateConfig,
  )
where

import Options.Applicative
import Vimeta.Core

-- | Common command line options among all of the apps.
data CommonOptions = CommonOptions
  { optsVerbose :: Bool,
    optsDryRun :: Bool
  }

-- | Common option parser.
commonOptions :: Parser CommonOptions
commonOptions =
  CommonOptions <$> switch infoVerbose
    <*> switch infoDryRun
  where
    infoVerbose = long "verbose" <> help "Enable verbose output"
    infoDryRun =
      short 'd' <> long "dry-run"
        <> help "Don't tag files, implies --verbose"

-- | Update the configuration file base on the common command line options.
updateConfig :: CommonOptions -> Config -> Config
updateConfig o c =
  c
    { configVerbose = configVerbose c || optsVerbose o,
      configDryRun = configDryRun c || optsDryRun o
    }
