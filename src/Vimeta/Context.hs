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
       , runVimeta
       , ask
       , asks
       , liftIO
       ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Reader
import Network.HTTP.Client (Manager, withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

--------------------------------------------------------------------------------
data Context = Context
  { ctxManager :: Manager
  }

--------------------------------------------------------------------------------
newtype Vimeta a = Vimeta {unV :: ReaderT Context IO a}
                 deriving (Functor, Applicative, Monad, MonadIO,
                           MonadReader Context)

--------------------------------------------------------------------------------
context :: Manager -> Context
context man = Context man

--------------------------------------------------------------------------------
runVimeta :: Vimeta a -> IO a
runVimeta m = withManager tlsManagerSettings go
  where go manager = runReaderT (unV m) (context manager)
