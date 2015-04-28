{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Utility functions for downloading files.
module Vimeta.Download
       ( withArtwork
       , withDownload
       ) where

--------------------------------------------------------------------------------
-- | Imports.
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HC
import System.FilePath
import System.IO (Handle, hFlush)
import System.IO.Temp (withSystemTempFile)
import Vimeta.Context

--------------------------------------------------------------------------------
-- | Try to download artwork and run the given function.  The
-- function will be passed a 'FilePath' if the artwork was downloaded.
withArtwork :: (MonadIO m) => [Text] -> (Maybe FilePath -> IO a) -> Vimeta m a
withArtwork []   f = liftIO $ f Nothing
withArtwork urls f = withDownload (listToMaybe $ candidates urls) f
  where
    candidates :: [Text] -> [Text]
    candidates = filter checkExtension . reverse

    checkExtension :: Text -> Bool
    checkExtension = goodExtension . takeExtension . Text.unpack . Text.toLower

    goodExtension :: String -> Bool
    goodExtension ext = ext == ".jpg" || ext == ".png"

--------------------------------------------------------------------------------
-- | Download the given URL to a temporary file and pass the file
-- name to the given function.
withDownload :: (MonadIO m)
             => Maybe Text
             -- ^ URL.

             -> (Maybe FilePath -> IO a)
             -- ^ Function to call and pass the file name to.

             -> Vimeta m a
             -- ^ Result of above function.

withDownload Nothing    f = liftIO $ f Nothing
withDownload (Just url) f = do
  manager <- asks ctxManager

  liftIO $ withSystemTempFile "vimeta" $ \name handle -> do
    downloadToHandle manager (Text.unpack url) handle
    f (Just name)

--------------------------------------------------------------------------------
-- | Helper function to the actual HTTP downloading into a file handle.
downloadToHandle :: HC.Manager -> String -> Handle -> IO ()
downloadToHandle manager url handle = do
  request  <- HC.parseUrl url
  response <- HC.httpLbs request manager
  BS.hPut handle (HC.responseBody response)
  hFlush handle
