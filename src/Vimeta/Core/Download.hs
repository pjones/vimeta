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
-- Utility functions for downloading files.
module Vimeta.Core.Download
  ( withArtwork,
    withDownload,
  )
where

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HC
import System.FilePath
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)
import Vimeta.Core.Config
import Vimeta.Core.Vimeta

-- | Try to download artwork and run the given function.  The
-- function will be passed a 'FilePath' if the artwork was downloaded.
--
-- See the 'withDownload' function for more details.
withArtwork ::
  (MonadIO m) =>
  [Text] ->
  (Maybe FilePath -> Vimeta IO a) ->
  Vimeta m a
withArtwork urls = withDownload (listToMaybe $ candidates urls)
  where
    candidates :: [Text] -> [Text]
    candidates = filter checkExtension . reverse
    checkExtension :: Text -> Bool
    checkExtension = goodExtension . takeExtension . toString . Text.toLower
    goodExtension :: String -> Bool
    goodExtension ext = ext == ".jpg" || ext == ".png"

-- | Download the given URL to a temporary file and pass the file
-- name to the given function.
--
-- The reason a function needs to be passed to 'withDownload' is the
-- result of using 'withSystemTempFile' to store the downloaded file.
-- The file will be automatically removed after the given function
-- completes.
withDownload ::
  (MonadIO m) =>
  -- | URL.
  Maybe Text ->
  -- | Function to call and pass the file name to.
  (Maybe FilePath -> Vimeta IO a) ->
  -- | Result of above function.
  Vimeta m a
withDownload Nothing f = do
  verbose "no URL to download"
  runIOE $ runVimeta (f Nothing)
withDownload url f = do
  context <- ask

  let dryRun = configDryRun $ ctxConfig context
      manager = ctxManager context

  case (dryRun, url) of
    (True, Nothing) ->
      verbose "dry-run: nothing to download"
        >> runWithoutTempFile f
    (False, Nothing) ->
      verbose "nothing to download"
        >> runWithoutTempFile f
    (True, Just u) ->
      verbose ("dry-run:" <> u)
        >> runWithoutTempFile f
    (False, Just u) ->
      verbose u
        >> runWithTempFile u manager f

-- | Helper function to run the download action with a temporary file.
runWithTempFile ::
  (MonadIO m) =>
  Text ->
  HC.Manager ->
  (Maybe FilePath -> Vimeta IO a) ->
  Vimeta m a
runWithTempFile url manager vio = do
  context <- ask

  runIOE $
    withSystemTempFile "vimeta" $ \name h -> do
      downloadToHandle manager (toString url) h
      execVimetaWithContext context $ vio (Just name)

-- | Helper function to run an action without needing a temporary file.
runWithoutTempFile ::
  (MonadIO m) =>
  (Maybe FilePath -> Vimeta IO a) ->
  Vimeta m a
runWithoutTempFile vio = do
  context <- ask
  runIOE $ execVimetaWithContext context $ vio Nothing

-- | Helper function to the actual HTTP downloading into a file handle.
downloadToHandle :: HC.Manager -> String -> Handle -> IO ()
downloadToHandle manager url handle = do
  request <- HC.parseRequest url
  response <- HC.httpLbs request manager
  LByteString.hPut handle (HC.responseBody response)
  hFlush handle
