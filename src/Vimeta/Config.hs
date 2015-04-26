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
-- | The configuration file.
module Vimeta.Config
       ( Config (..)
       , defaultConfig
       , configFileName
       , readConfig
       , writeConfig
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, encodeFile)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (takeDirectory)
import Vimeta.Tagger

--------------------------------------------------------------------------------
-- | Vimeta configuration.
data Config = Config
  { configTMDBKey     :: Text
  , configFormatMovie :: Text
  , configFormatTV    :: Text
  }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "tmdb_key"
           <*> v .: "cmd_movie"
           <*> v .: "cmd_tv"
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
         }

  where (fmtMovie, fmtTV) = formatStringsForTagger tagger

--------------------------------------------------------------------------------
-- | Get the name of the configuration file.
configFileName :: IO FilePath
configFileName = getUserConfigFile "vimeta" "config.yml"

--------------------------------------------------------------------------------
-- | Read the configuration file and return a 'Config' value or an error.
readConfig :: IO (Either String Config)
readConfig = do
  filename <- configFileName
  exists   <- doesFileExist filename

  if exists
    then decodeConfig filename
    else return (Left $ missingFile filename)

  where
    decodeConfig :: FilePath -> IO (Either String Config)
    decodeConfig fn = do result <- decodeFileEither fn
                         return $ case result of
                                    Left e  -> Left (show e)
                                    Right a -> Right a

    missingFile :: FilePath -> String
    missingFile fn = "no config file found, use the `config' command " ++
                     "to create " ++ fn

--------------------------------------------------------------------------------
writeConfig :: Config -> IO (Either String FilePath)
writeConfig c = do
  filename <- configFileName
  exists   <- doesFileExist filename

  -- mkdir -p `dirname filename`
  createDirectoryIfMissing True (takeDirectory filename)

  if exists
    then return (Left $ existError filename)
    else encodeFile filename c >> return (Right filename)

  where
    existError :: FilePath -> String
    existError fn = "please remove the existing config file first: " ++ fn
