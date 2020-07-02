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

-- | Caching functions.
module Vimeta.Core.Cache
  ( cacheTMDBConfig,
  )
where

import Control.Monad (liftM)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Time.Calendar
import Data.Time.Clock
import qualified Network.API.TheMovieDB as TheMovieDB
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.Environment.XDG.BaseDir (getUserCacheFile)
import System.FilePath (takeDirectory)

-- | Manage cache file expiration.
data Age
  = -- | Cap to N days.
    MaxDays Int

ageAsTime :: Age -> UTCTime -> UTCTime
ageAsTime (MaxDays days) now =
  now {utctDay = addDays (fromIntegral (- days)) (utctDay now)}

-- | The file name for catching @TheMovieDB.Configuration@.
tmdbCacheFile :: IO FilePath
tmdbCacheFile = getUserCacheFile "vimeta" "tmdb-config.json"

-- | Produce a cached version of @TheMovieDB.Configuration@ or use
-- the given action to create a cache a new value.
cacheTMDBConfig ::
  (MonadIO m) =>
  m (Either e TheMovieDB.Configuration) ->
  m (Either e TheMovieDB.Configuration)
cacheTMDBConfig action = do
  file <- liftIO tmdbCacheFile
  cache file (MaxDays 3) action

-- | Generic cache reader.
readCache :: (MonadIO m, FromJSON a) => FilePath -> Age -> m (Maybe a)
readCache filename age = do
  exists <- liftIO (doesFileExist filename)
  if not exists then return Nothing else go
  where
    go = do
      now <- liftIO getCurrentTime
      modtime <- liftIO (getModificationTime filename)

      if fresh now modtime
        then Aeson.decode' `liftM` liftIO (BL.readFile filename)
        else return Nothing
    fresh :: UTCTime -> UTCTime -> Bool
    fresh now modtime = ageAsTime age now <= modtime

-- | Generic cache writer.
writeCache :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeCache filename value = liftIO $ do
  createDirectoryIfMissing True (takeDirectory filename)
  BL.writeFile filename (Aeson.encode value)

-- | Generic caching function.
cache ::
  (MonadIO m, FromJSON a, ToJSON a) =>
  -- | Cache file.
  FilePath ->
  -- | Age of cache file.
  Age ->
  -- | Action to generate new value.
  m (Either e a) ->
  -- | Cached or new value.
  m (Either e a)
cache file age action = do
  cached <- liftIO (readCache file age)

  case cached of
    Just c -> return (Right c)
    Nothing -> do
      result <- action
      either (const $ return ()) (writeCache file) result
      return result
