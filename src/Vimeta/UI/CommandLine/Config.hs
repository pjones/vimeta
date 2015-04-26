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
module Vimeta.UI.CommandLine.Config
       ( Options
       , optionsParser
       , run
       ) where

--------------------------------------------------------------------------------
import Control.Monad.Error
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Console.Byline
import Vimeta.Config
import Vimeta.Tagger

--------------------------------------------------------------------------------
data Options = Options
  { optsKey    :: Maybe Text
  , optsTagger :: Tagger
  }

--------------------------------------------------------------------------------
optionsParser :: Parser Options
optionsParser = Options <$> (optional $ Text.pack <$> strOption getKey)
                        <*> pure AtomicParsley

  where
    -- Parser options for @optsKey@
    getKey = mconcat [ short 'k'
                     , long "key"
                     , metavar "KEY"
                     , help "Set the API key to KEY"
                     ]

--------------------------------------------------------------------------------
run :: Options -> IO (Either String ())
run opts = do
  let def    = defaultConfig (optsTagger opts)
      config = case optsKey opts of
                 Nothing -> def
                 Just k  -> def {configTMDBKey = k}

  runErrorT $ do
    result <- ErrorT $ writeConfig config

    case optsKey opts of
      Just _  -> return ()
      Nothing -> liftIO (reportMissing result)

  where
    reportMissing fn = runByline (reportLn Warning $ missingKey fn)
    missingKey fn = "please edit the config file and set the API key: " <>
                    text (Text.pack fn)
