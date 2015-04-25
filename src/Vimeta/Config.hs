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
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
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
defaultConfig :: Tagger -> Config
defaultConfig tagger =
  Config { configTMDBKey     = "your API key goes here"
         , configFormatMovie = fmtMovie
         , configFormatTV    = fmtTV
         }

  where (fmtMovie, fmtTV) = formatStringsForTagger tagger
