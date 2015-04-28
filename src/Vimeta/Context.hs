{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
       , byline
       , tmdb
       , runVimeta
       ,runVimetaBylineApp
       , ask
       , asks
       , liftIO
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import qualified Data.Text as Text
import Network.API.TheMovieDB (TheMovieDB, Key, runTheMovieDBWithManager)
import qualified Network.API.TheMovieDB as TheMovieDB
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Console.Byline hiding (ask)
import System.Exit
import Vimeta.Config

--------------------------------------------------------------------------------
data Context = Context
  { ctxManager :: Manager
  , ctxConfig  :: Config
  , ctxTMDBCfg :: TheMovieDB.Configuration
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
loadTMDBConfig :: (MonadIO m) => Manager -> Key -> EitherT String m TheMovieDB.Configuration
loadTMDBConfig manager key = do
  -- FIXME: Cache the config value
  result <- liftIO $ runTheMovieDBWithManager manager key TheMovieDB.config

  case result of
    Left e  -> left (show e)
    Right c -> return c

--------------------------------------------------------------------------------
-- | Run a 'Vimeta' operation.
runVimeta :: (MonadIO m) => Vimeta m a -> m (Either String a)
runVimeta vimeta = runEitherT $ do
  config <- readConfig
  manager <- liftIO $ newManager tlsManagerSettings
  tc <- loadTMDBConfig manager (configTMDBKey config)
  runReaderT (unV vimeta) (Context manager config tc)

--------------------------------------------------------------------------------
runVimetaBylineApp :: Vimeta (Byline IO) () -> IO ()
runVimetaBylineApp vimeta = runByline $ do
    v <- runVimeta vimeta

    case v of
      Right _ -> liftIO exitSuccess
      Left  e -> reportLn Error (text $ Text.pack e) >> liftIO exitFailure
