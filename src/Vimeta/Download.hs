{-

This file is part of the Haskell package vimeta. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/vimeta/LICENSE. No part of
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

module Vimeta.Download (download) where
import System.IO.Temp (withSystemTempFile)
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkHandle)
import Network.HTTP.Conduit (Response(..), parseUrl, withManager, http)
import System.IO

download :: Maybe String -> (Maybe FilePath -> IO a) -> IO a
download url f = case url of
  Nothing   -> f Nothing
  Just url' -> withSystemTempFile "vimeta.img" $
               \name handle -> downloadTo url' handle >> f (Just name)

downloadTo :: String -> Handle -> IO ()
downloadTo url handle = do request <- parseUrl url
                           withManager $ \manager -> do
                             Response _ _ _ src <- http request manager
                             src $$+- sinkHandle handle
