{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}

{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | The configuration file.
module Vimeta.Core.Config
       ( Config (..)
       , defaultConfig
       , configFileName
       , readConfig
       , writeConfig
       ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, encodeFile)
import Network.API.TheMovieDB (Key)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (takeDirectory)

--------------------------------------------------------------------------------
-- Local imports:
import Vimeta.Core.Tagger

--------------------------------------------------------------------------------
-- The following is a kludge to avoid the "redundant import" warning
-- when using GHC >= 7.10.x.  This should be removed after we decide
-- to stop supporting GHC < 7.10.x.
import Prelude

--------------------------------------------------------------------------------
-- | Vimeta configuration.
data Config = Config
  { configTMDBKey     :: Key
  , configFormatMovie :: Text
  , configFormatTV    :: Text
  , configVerbose     :: Bool
  , configDryRun      :: Bool
  }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .:  "tmdb_key"
           <*> v .:  "cmd_movie"
           <*> v .:  "cmd_tv"
           <*> v .:? "verbose" .!= False
           <*> v .:? "dryrun"  .!= False
  parseJSON x = typeMismatch "configuration" x

--------------------------------------------------------------------------------
instance ToJSON Config where
  toJSON c = object [ "tmdb_key"  .= configTMDBKey c
                    , "cmd_movie" .= configFormatMovie c
                    , "cmd_tv"    .= configFormatTV c
                    ]

--------------------------------------------------------------------------------
defaultConfig :: Tagger -> Config
defaultConfig tagger =
  Config { configTMDBKey     = "your API key goes here"
         , configFormatMovie = fmtMovie
         , configFormatTV    = fmtTV
         , configVerbose     = False
         , configDryRun      = False
         }

  where (fmtMovie, fmtTV) = formatStringsForTagger tagger

--------------------------------------------------------------------------------
-- | Get the name of the configuration file.
configFileName :: IO FilePath
configFileName = getUserConfigFile "vimeta" "config.yml"

--------------------------------------------------------------------------------
-- | Read the configuration file and return a 'Config' value or an error.
readConfig :: (MonadIO m) => EitherT String m Config
readConfig = do
  filename <- liftIO configFileName
  exists   <- liftIO (doesFileExist filename)

  if exists
    then decodeConfig filename
    else left $ missingFile filename

  where
    decodeConfig :: (MonadIO m) => FilePath -> EitherT String m Config
    decodeConfig fn = do result <- liftIO $ decodeFileEither fn
                         case result of
                           Left e  -> left (show e)
                           Right a -> return a

    missingFile :: FilePath -> String
    missingFile fn = "no config file found, use the `config' command " ++
                     "to create " ++ fn

--------------------------------------------------------------------------------
writeConfig :: (MonadIO m) => Config -> EitherT String m FilePath
writeConfig c = do
  (filename, exists) <- liftIO $ do
    fn <- configFileName
    ex <- doesFileExist fn
    return (fn, ex)

  when exists $ left (existError filename)

  liftIO (createDirectoryIfMissing True (takeDirectory filename))
  liftIO (encodeFile filename c)
  return filename

  where
    existError :: FilePath -> String
    existError fn = "please remove the existing config file first: " ++ fn
