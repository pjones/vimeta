{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.TV (Config(..), update) where
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (unpack)
import System.Exit (exitFailure)
import Vimeta.Download (download)
import qualified Network.API.TheTVDB as TVDB
import qualified Network.API.TheTVDB.Zipper as Z
import qualified Vimeta.AtomicParsley as AP

data Config = Config
  { seriesID              :: TVDB.UniqueID
  , seasonNumber          :: TVDB.UniqueID
  , startingEpisodeNumber :: Maybe TVDB.UniqueID
  , episodeFiles          :: [FilePath]
  } deriving (Eq, Show)


atomicParsleyOptions :: Z.Zipper -> Maybe FilePath -> AP.Options
atomicParsleyOptions z poster =
  [ ("--stik",         "TV Show")
  , ("--year",         maybe "" AP.formatDate $ TVDB.episodeDate episode)
  , ("--title",        unpack $ TVDB.episodeName episode)
  , ("--description",  unpack $ TVDB.episodeOverview episode)
  , ("--TVShowName",   unpack $ TVDB.seriesName series)
  , ("--TVSeasonNum",  show $ TVDB.seasonNumber season)
  , ("--TVEpisodeNum", show $ TVDB.episodeNumber episode)
  , ("--tracknum",     show $ TVDB.episodeNumber episode)
  , ("--artwork",      fromMaybe "REMOVE_ALL" poster)
  ]
  where series  = Z.series z
        season  = Z.season z
        episode = Z.episode z

update :: Config -> IO ()
update c =
  do key <- TVDB.loadKeyMaybe >>= maybe keyErr return
     ctx <- TVDB.defaultContext key
     tv  <- TVDB.fetch ctx (seriesID c)
     case Z.find tv (seasonNumber c) episodeNumber of
       Nothing -> fail epErr
       Just z  -> download (fmap unpack $ TVDB.seriesPosterURL tv) (updateAll z)
  where episodeNumber = fromMaybe 1 (startingEpisodeNumber c)
        keyErr = fail "failed to load TheTVDB API key"
        epErr = "failed to find episode number " ++ show episodeNumber
        updateAll z p = mapM_ (uncurry $ updateWithMetadata p) (zipped z)
        zipped z = zip (Z.episodes z) (episodeFiles c)

updateWithMetadata :: Maybe FilePath -> Z.Zipper -> FilePath -> IO ()
updateWithMetadata poster z file =
  do putStrLn $ file ++ " --> " ++ unpack (TVDB.episodeName episode)
     AP.update file $ atomicParsleyOptions z poster
  where episode = Z.episode z
