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
       , die
       , runVimeta
       , ask
       , asks
       , liftIO
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.HTTP.Client (Manager, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Vimeta.Config

--------------------------------------------------------------------------------
data Context = Context
  { ctxManager :: Manager
  , ctxConfig  :: Config
  }

--------------------------------------------------------------------------------
newtype Vimeta a =
  Vimeta {unV :: ReaderT Context (EitherT String IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

--------------------------------------------------------------------------------
context :: Manager -> Config -> Context
context man cfg = Context man cfg

--------------------------------------------------------------------------------
-- | Terminate a 'Vimeta' session with an error message.
die :: String -> Vimeta a
die message = Vimeta $ lift (left message)

--------------------------------------------------------------------------------
runVimeta :: Vimeta a -> IO (Either String a)
runVimeta m = do
  configE <- readConfig

  case configE of
    Left e    -> return (Left e)
    Right cfg -> withManager tlsManagerSettings (go cfg)

  where
    go config manager =
      let ctx = context manager config
      in runEitherT $ runReaderT (unV m) ctx
