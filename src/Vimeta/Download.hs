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
module Vimeta.Download (download) where

--------------------------------------------------------------------------------
-- | Imports.
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Client as HC
import System.IO (Handle, hFlush)
import System.IO.Temp (withSystemTempFile)
import Vimeta.Context

--------------------------------------------------------------------------------
-- | Download the given URL to a temporary file and pass the file
-- name to the given function.
download :: Maybe String
            -- ^ URL.

         -> (Maybe FilePath -> Vimeta a)
         -- ^ Function to call and pass the file name to.

         -> Vimeta a
         -- ^ Result of above function.

download Nothing    f = f Nothing
download (Just url) f = do
  manager <- asks ctxManager

  name <- liftIO $ withSystemTempFile "vimeta" $ \name handle -> do
    downloadToHandle manager url handle
    return name

  f (Just name)

--------------------------------------------------------------------------------
-- | Helper function to the actual HTTP downloading into a file handle.
downloadToHandle :: HC.Manager -> String -> Handle -> IO ()
downloadToHandle manager url handle = do
  request  <- HC.parseUrl url
  response <- HC.httpLbs request manager
  BS.hPut handle (HC.responseBody response)
  hFlush handle
