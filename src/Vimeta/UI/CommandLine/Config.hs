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
import Control.Monad
import Control.Monad.Trans.Either
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative
import System.Console.Byline
import System.Exit
import Vimeta.Core

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
data Options = Options
  { optsKey    :: Maybe Text
  , optsTagger :: Tagger
  }

--------------------------------------------------------------------------------
optionsParser :: Parser Options
optionsParser = Options <$> optional (Text.pack <$> strOption getKey)
                        <*> pure AtomicParsley

  where
    -- Parser options for @optsKey@
    getKey = mconcat [ short 'k'
                     , long "key"
                     , metavar "KEY"
                     , help "Set the API key to KEY"
                     ]

--------------------------------------------------------------------------------
run :: Options -> IO ()
run opts = do
  let def    = defaultConfig (optsTagger opts)
      config = case optsKey opts of
                 Nothing -> def
                 Just k  -> def {configTMDBKey = k}

  result <- runEitherT (app opts config)

  case result of
    Left e         -> byline Error e >> exitFailure
    Right (Just w) -> byline Warning w
    Right Nothing  -> return ()

  where
    byline :: ReportType -> String -> IO ()
    byline rt = void . runByline . reportLn rt . text . Text.pack

--------------------------------------------------------------------------------
app :: Options -> Config -> EitherT String IO (Maybe String)
app opts config = do
  filename <- writeConfig config

  return $ case optsKey opts of
    Just _  -> Nothing -- No warnings.
    Nothing -> Just (missingKey filename)

  where
    missingKey = ("please edit the config file and set the API key: " ++)
