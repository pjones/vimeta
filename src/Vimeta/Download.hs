{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.Download (download) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types
import System.IO
import System.IO.Temp (withSystemTempFile)

--------------------------------------------------------------------------------
download :: Maybe String              -- ^ URL.
         -> (Maybe FilePath -> IO a)  -- ^ Function to run on downloaded file.
         -> IO a                      -- ^ Result of above function.
download url f =
  case url of
    Nothing   -> f Nothing
    Just url' -> withSystemTempFile "vimeta.img" $ \name handle -> $ do
      downloadTo url' handle
      f (Just name)

--------------------------------------------------------------------------------
-- FIXME: Don't create a new manager all the time.
downloadTo :: String -> Handle -> IO ()
downloadTo url handle = do
  request <- HC.parseUrl url
  withManager $ \manager -> do
    response <- HC.httpLbs request manager
    BS.hPut handle (HC.responseBody r)
    hFlush handle
