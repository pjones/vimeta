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
module Vimeta.UI.Common.TV
  ( EpisodeSpec (..),
    tagWithMappingFile,
    tagWithSpec,
    tagWithFileOrder,
    episodeSpec,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Network.API.TheMovieDB
import Text.Parsec
import Vimeta.Core
import qualified Vimeta.Core.MappingFile as MF

-- | A simple way to specify a single episode.
data EpisodeSpec = EpisodeSpec Int Int deriving (Show, Eq, Ord)

-- | An episode along with the season.
data EpisodeCtx = EpisodeCtx TV Season Episode deriving (Show, Eq, Ord)

-- | Tag a single file with the given 'EpisodeCtx'.
tagFileWithEpisode :: (MonadIO m) => FilePath -> EpisodeCtx -> Vimeta m ()
tagFileWithEpisode file (EpisodeCtx tv season episode) = do
  context <- ask

  let format = configFormatTV (ctxConfig context)
      tmdbCfg = ctxTMDBCfg context

  withArtwork (seasonPosterURLs tmdbCfg season) $ \artwork ->
    case fromFormatString (formatMap artwork) "config.tv" format of
      Left e -> throwError e
      Right cmd -> tagFile cmd
  where
    formatMap :: Maybe FilePath -> FormatTable
    formatMap artwork =
      Map.fromList
        [ ('Y', formatFullDate $ episodeAirDate episode),
          ('a', Text.pack <$> artwork),
          ('d', Just (Text.take 255 $ episodeOverview episode)),
          ('e', Just . Text.pack . show $ episodeNumber episode),
          ('f', Just $ Text.pack file),
          ('n', Just $ tvName tv),
          ('s', Just . Text.pack . show $ episodeSeasonNumber episode),
          ('t', Just $ episodeName episode),
          ('y', formatYear $ episodeAirDate episode)
        ]

-- | Handy tagging function using mapping files.
tagWithMappingFile :: (MonadIO m) => TV -> FilePath -> Vimeta m ()
tagWithMappingFile tv filename = do
  mapping <- parseMappingFile filename episodeSpecParser
  tagWithSpec tv mapping

-- | Tag all of the given files with their matching 'EpisodeSpec'.
tagWithSpec ::
  (MonadIO m) =>
  -- | Full TV series.
  TV ->
  -- | File mapping.
  [(FilePath, EpisodeSpec)] ->
  Vimeta m ()
tagWithSpec tv specs = do
  let unmapped = lefts mapping
      taggable = rights mapping

  unless (null unmapped) $
    throwError
      ( "the following files can't be mapped to episodes "
          <> Text.unpack (badFiles unmapped)
      )

  mapM_ (uncurry tagFileWithEpisode) taggable
  where
    table :: Map EpisodeSpec EpisodeCtx
    table = makeTVMap tv
    mapping :: [Either (FilePath, EpisodeSpec) (FilePath, EpisodeCtx)]
    mapping = map (\(f, s) -> check (Map.lookup s table) f s) specs
    check ::
      Maybe EpisodeCtx ->
      FilePath ->
      EpisodeSpec ->
      Either (FilePath, EpisodeSpec) (FilePath, EpisodeCtx)
    check Nothing f s = Left (f, s)
    check (Just e) f _ = Right (f, e)
    badFiles :: [(FilePath, EpisodeSpec)] -> Text
    badFiles =
      Text.intercalate "\n"
        . map (\(f, s) -> Text.pack f <> " " <> episodeSpecAsText s)

-- | Tag the given files, starting at the given 'EpisodeSpec'.
tagWithFileOrder ::
  (MonadIO m) =>
  -- | Full TV series.
  TV ->
  -- | Starting episode.
  EpisodeSpec ->
  -- | List of files to tag.
  [FilePath] ->
  Vimeta m ()
tagWithFileOrder tv spec files = tagWithSpec tv mapping
  where
    mapping :: [(FilePath, EpisodeSpec)]
    mapping = zipWith (\f e -> (f, episodeSpecFromCtx e)) files episodes
    episodes :: [EpisodeCtx]
    episodes = take (length files) $ startingAt spec $ flattenTV tv

-- | Create an 'EpisodeSpec' from an 'Episode'.
episodeSpec :: Episode -> EpisodeSpec
episodeSpec e = EpisodeSpec (episodeSeasonNumber e) (episodeNumber e)

-- | Create an 'EpisodeSpec' from an 'EpisodeCtx'.
episodeSpecFromCtx :: EpisodeCtx -> EpisodeSpec
episodeSpecFromCtx (EpisodeCtx _ _ e) = episodeSpec e

-- | Turn an 'EpisodeSpec' into something that can be printed.
episodeSpecAsText :: EpisodeSpec -> Text
episodeSpecAsText (EpisodeSpec s e) =
  "S" <> Text.pack (show s)
    <> "E"
    <> Text.pack (show e)

-- | Flatten a TV/Season/Episode tree into a list of episodes.
flattenTV :: TV -> [EpisodeCtx]
flattenTV t = concatMap (\s -> forSeason s (seasonEpisodes s)) (tvSeasons t)
  where
    forSeason :: Season -> [Episode] -> [EpisodeCtx]
    forSeason s = map (EpisodeCtx t s)

-- | Drop all episodes until the matching 'EpisodeSpec' is found.
startingAt :: EpisodeSpec -> [EpisodeCtx] -> [EpisodeCtx]
startingAt spec = dropWhile (\(EpisodeCtx _ _ e) -> spec /= episodeSpec e)

-- | Make an episode look-up table.
makeTVMap :: TV -> Map EpisodeSpec EpisodeCtx
makeTVMap = foldr insert Map.empty . flattenTV
  where
    insert :: EpisodeCtx -> Map EpisodeSpec EpisodeCtx -> Map EpisodeSpec EpisodeCtx
    insert e = Map.insert (episodeSpecFromCtx e) e

episodeSpecParser :: MF.Parser EpisodeSpec
episodeSpecParser = go <?> "episode spec (S#E#)"
  where
    go = do
      void (oneOf "Ss")
      season <- num <?> "season number"

      void (oneOf "Ee")
      episode <- num <?> "episode number"

      return $ EpisodeSpec season episode
    num =
      many1 digit
        <&> readMaybe
        >>= maybe (fail "invalid number") pure
