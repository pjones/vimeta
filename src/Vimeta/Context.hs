{-# LANGUAGE GeneralizedNewtypeDeriving,ScopedTypeVariables #-}

{-

This file is part of the vimeta package. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/vimeta/LICENSE. No part of the
vimeta package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Vimeta.Context
       ( Vimeta (..)
       , Context (..)
       , MonadIO
       , die
       , runIO
       , runIOE
       , byline
       , tmdb
       , verbose
       , execVimetaWithContext
       , execVimeta
       , execVimetaBylineApp
       , runVimeta
       , ask
       , asks
       , liftIO
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Network.API.TheMovieDB (TheMovieDB, Key, runTheMovieDBWithManager)
import qualified Network.API.TheMovieDB as TheMovieDB
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Console.Byline hiding (ask)
import System.Exit (exitSuccess, exitFailure)
import System.IO (Handle, stdout)
import Vimeta.Config

--------------------------------------------------------------------------------
data Context = Context
  { ctxManager  :: Manager
  , ctxConfig   :: Config
  , ctxTMDBCfg  :: TheMovieDB.Configuration
  , ctxVerboseH :: Handle
  }

--------------------------------------------------------------------------------
newtype Vimeta m a =
  Vimeta {unV :: ReaderT Context (EitherT String m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

--------------------------------------------------------------------------------
-- | Terminate a 'Vimeta' session with an error message.
die :: (Monad m) => String -> Vimeta m a
die message = Vimeta $ lift (left message)

--------------------------------------------------------------------------------
runIO :: (MonadIO m) => IO a -> Vimeta m a
runIO io = liftIO (try io) >>= sinkIO
  where sinkIO :: (Monad m) => Either SomeException a -> Vimeta m a
        sinkIO (Left e)  = die (show e)
        sinkIO (Right a) = return a

--------------------------------------------------------------------------------
runIOE :: (MonadIO m) => IO (Either String a) -> Vimeta m a
runIOE io = runIO io >>= either (die . show) return

--------------------------------------------------------------------------------
-- | Run a 'Byline' operation.
byline :: Byline IO a -> Vimeta (Byline IO) a
byline = Vimeta . lift . lift

--------------------------------------------------------------------------------
-- | Run a 'TheMovieDB' operation.
tmdb :: (MonadIO m) => TheMovieDB a -> Vimeta m a
tmdb t = do
  context' <- ask

  let manager = ctxManager context'
      key     = configTMDBKey (ctxConfig context')

  result <- liftIO (runTheMovieDBWithManager manager key t)

  case result of
    Left e  -> die (show e)
    Right r -> return r

--------------------------------------------------------------------------------
verbose :: (MonadIO m) => Text -> Vimeta m ()
verbose msg = do
  context <- ask

  let okay = configVerbose (ctxConfig context) ||
             configDryRun  (ctxConfig context)

  when okay $ liftIO $ Text.hPutStrLn (ctxVerboseH context) msg

--------------------------------------------------------------------------------
loadTMDBConfig :: (MonadIO m) => Manager -> Key -> EitherT String m TheMovieDB.Configuration
loadTMDBConfig manager key = do
  -- FIXME: Cache the config value
  result <- liftIO $ runTheMovieDBWithManager manager key TheMovieDB.config

  case result of
    Left e  -> left (show e)
    Right c -> return c

--------------------------------------------------------------------------------
-- | Very primitive way of running a 'Vimeta' value with the given 'Context'.
-- Mostly useful for running vimeta action within another vimeta
-- action.
execVimetaWithContext :: (MonadIO m)
                     => Context
                     -> Vimeta m a
                     -> m (Either String a)
execVimetaWithContext context vimeta =
  runEitherT $ runReaderT (unV vimeta) context

--------------------------------------------------------------------------------
-- | Run a 'Vimeta' operation after loading the configuration file
-- from disk.
execVimeta :: (MonadIO m)
           => (Config -> Config)  -- ^ Modify configuration before running.
           -> Vimeta m a          -- ^ The Vimeta value to execute.
           -> m (Either String a) -- ^ The result.
execVimeta cf vimeta = runEitherT $ do
  config <- cf <$> readConfig
  manager <- liftIO $ newManager tlsManagerSettings
  tc <- loadTMDBConfig manager (configTMDBKey config)
  EitherT $ execVimetaWithContext (Context manager config tc stdout) vimeta

--------------------------------------------------------------------------------
-- | Helper function to run a 'Vimeta' value based in 'Byline'.
execVimetaBylineApp :: (Config -> Config) -> Vimeta (Byline IO) () -> IO ()
execVimetaBylineApp cf vimeta = runByline $ do
    v <- execVimeta cf vimeta

    case v of
      Right _ -> liftIO exitSuccess
      Left  e -> reportLn Error (text $ Text.pack e) >> liftIO exitFailure

--------------------------------------------------------------------------------
-- | Simple wrapper around 'execVimeta'.
runVimeta :: (MonadIO m) => Vimeta m a -> m (Either String a)
runVimeta = execVimeta id
