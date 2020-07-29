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
module Vimeta.Core.Vimeta
  ( Vimeta (..),
    Context (..),
    MonadIO,
    throwError,
    runIO,
    runIOE,
    tmdb,
    verbose,
    execVimetaWithContext,
    execVimeta,
    runVimeta,
  )
where

import Byline (BylineT, MonadByline, runBylineT)
import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Text.IO as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Network.API.TheMovieDB as TMDb
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Vimeta.Core.Cache
import Vimeta.Core.Config

data Context = Context
  { ctxManager :: Manager,
    ctxConfig :: Config,
    ctxTMDBCfg :: TMDb.Configuration,
    ctxVerboseH :: Handle
  }

newtype Vimeta m a = Vimeta
  {unV :: ReaderT Context (BylineT (ExceptT String m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Context,
      MonadError String,
      MonadByline
    )

runIO :: (MonadIO m) => IO a -> Vimeta m a
runIO io = liftIO (try io) >>= sinkIO
  where
    sinkIO :: (Monad m) => Either SomeException a -> Vimeta m a
    sinkIO (Left e) = throwError (show e)
    sinkIO (Right a) = return a

runIOE :: (MonadIO m) => IO (Either String a) -> Vimeta m a
runIOE io = runIO io >>= either (throwError . show) return

-- | Run a 'TheMovieDB' operation.
tmdb :: (MonadIO m) => TMDb.TheMovieDB a -> Vimeta m a
tmdb t = do
  context' <- ask

  let manager = ctxManager context'
      key = configTMDBKey (ctxConfig context')
      settings = TMDb.defaultSettings key

  result <- liftIO (TMDb.runTheMovieDBWithManager manager settings t)

  case result of
    Left e -> throwError (show e)
    Right r -> return r

verbose :: (MonadIO m) => Text -> Vimeta m ()
verbose msg = do
  context <- ask

  let okay =
        configVerbose (ctxConfig context)
          || configDryRun (ctxConfig context)

  when okay $ liftIO $ Text.hPutStrLn (ctxVerboseH context) msg

loadTMDBConfig ::
  (MonadIO m) =>
  Manager ->
  TMDb.Settings ->
  ExceptT String m TMDb.Configuration
loadTMDBConfig manager settings = do
  result <-
    cacheTMDBConfig
      ( liftIO $ TMDb.runTheMovieDBWithManager manager settings TMDb.config
      )

  case result of
    Left e -> throwError (show e)
    Right c -> return c

-- | Very primitive way of running a 'Vimeta' value with the given 'Context'.
-- Mostly useful for running vimeta action within another vimeta
-- action.
execVimetaWithContext ::
  (MonadIO m, MonadMask m) =>
  Context ->
  Vimeta m a ->
  m (Either String a)
execVimetaWithContext context vimeta =
  unV vimeta
    & (`runReaderT` context)
    & runBylineT
    & (>>= maybe (throwError "EOF") pure)
    & runExceptT

-- | Force the current process to use UTF-8 for output.
forceUTF8 :: IO ()
forceUTF8 = setLocaleEncoding utf8

-- | Run a 'Vimeta' operation after loading the configuration file
-- from disk.
execVimeta ::
  (MonadIO m, MonadMask m) =>
  -- | Modify configuration before running.
  (Config -> Config) ->
  -- | The Vimeta value to execute.
  Vimeta m a ->
  -- | The result.
  m (Either String a)
execVimeta cf vimeta = runExceptT $ do
  liftIO forceUTF8
  config <- cf <$> readConfig
  manager <- liftIO $ newManager tlsManagerSettings
  tc <- loadTMDBConfig manager (TMDb.defaultSettings (configTMDBKey config))
  ExceptT $ execVimetaWithContext (Context manager config tc stdout) vimeta

-- | Simple wrapper around 'execVimeta'.
runVimeta :: (MonadIO m, MonadMask m) => Vimeta m a -> m (Either String a)
runVimeta = execVimeta id
