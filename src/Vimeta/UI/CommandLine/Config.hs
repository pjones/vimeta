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
module Vimeta.UI.CommandLine.Config
  ( Options,
    optionsParser,
    run,
  )
where

import qualified Byline.Exit as B
import qualified Data.Text as Text
import Options.Applicative
import Vimeta.Core

data Options = Options
  { optsKey :: Maybe Text,
    optsTagger :: Tagger
  }

optionsParser :: Parser Options
optionsParser =
  Options <$> optional (Text.pack <$> strOption getKey)
    <*> pure AtomicParsley
  where
    -- Parser options for @optsKey@
    getKey =
      mconcat
        [ short 'k',
          long "key",
          metavar "KEY",
          help "Set the API key to KEY"
        ]

run :: Options -> IO ()
run opts = do
  let def = defaultConfig (optsTagger opts)
      config = case optsKey opts of
        Nothing -> def
        Just k -> def {configTMDBKey = k}

  result <- runExceptT (app opts config)

  case result of
    Left e -> B.die (B.text $ toText e)
    Right (Just msg) -> B.warn (B.text $ toText msg)
    Right Nothing -> return ()

app :: Options -> Config -> ExceptT String IO (Maybe String)
app opts config = do
  filename <- writeConfig config

  return $ case optsKey opts of
    Just _ -> Nothing -- No warnings.
    Nothing -> Just (missingKey filename)
  where
    missingKey = ("please edit the config file and set the API key: " ++)
