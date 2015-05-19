{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

This file is part of the package byline. It is subject to the license
terms in the LICENSE file found in the top-level directory of this
distribution and at git://pmade.com/byline/LICENSE. No part of the
byline package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module System.Console.Byline.Internal.Byline
       ( Byline (..)
       , Env    (..)
       , eof
       , liftOuter
       , liftInputT
       , runByline
       ) where


--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.IORef
import System.Console.Byline.Internal.Completion
import System.Console.Byline.Internal.Render
import System.Environment (lookupEnv)
import System.IO (Handle, stdout)
import qualified System.Terminfo as Term
import qualified System.Terminfo.Caps as Term

--------------------------------------------------------------------------------
-- Import Haskeline, which is used to do the heavy lifting.
import qualified System.Console.Haskeline    as H
import qualified System.Console.Haskeline.IO as H

--------------------------------------------------------------------------------
data Env = Env
  { sayMode    :: RenderMode
  , askMode    :: RenderMode
  , outHandle  :: Handle
  , inputState :: H.InputState
  , compFunc   :: IORef (Maybe CompletionFunc)
  }

--------------------------------------------------------------------------------
-- | Reader environment for Byline.
newtype Byline m a = Byline {unByline :: ReaderT Env (MaybeT m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

--------------------------------------------------------------------------------
defRenderMode :: H.InputT IO (RenderMode, RenderMode)
defRenderMode = do
  termHint  <- H.haveTerminalUI
  maxColors <- liftIO (runMaybeT getMaxColors)

  return $ case (termHint, maxColors) of
             (True, Just n) | n < 256   -> (Simple,  Simple)
                            | otherwise -> (Term256, Term256)
             (True, Nothing)            -> (Simple,  Plain)
             (False, _)                 -> (Plain,   Plain)
  where
    getMaxColors :: MaybeT IO Int
    getMaxColors = do
      term <- MaybeT (lookupEnv "TERM")
      db   <- liftIO (Term.acquireDatabase term)

      case db of
        Left _  -> MaybeT (return Nothing)
        Right d -> MaybeT (return $ Term.queryNumTermCap d Term.MaxColors)

--------------------------------------------------------------------------------
-- | Create the default reader environment.
defEnv :: H.InputState
       -> (RenderMode, RenderMode)
       -> IORef (Maybe CompletionFunc)
       -> Env
defEnv state (smode, amode) comp =
  Env { sayMode    = smode
      , askMode    = amode
      , outHandle  = stdout
      , inputState = state
      , compFunc   = comp
      }

--------------------------------------------------------------------------------
eof :: (Monad m) => Byline m a
eof = Byline $ lift (MaybeT $ return Nothing)

--------------------------------------------------------------------------------
liftOuter :: (Monad m) => m a -> Byline m a
liftOuter = Byline . lift . lift

--------------------------------------------------------------------------------
-- | Lift an 'InputT' action into 'Byline'.
liftInputT :: (MonadIO m) => H.InputT IO a -> Byline m a
liftInputT input = do
  state <- asks inputState
  liftIO (H.queryInput state $ H.withInterrupt input)

--------------------------------------------------------------------------------
runByline :: (MonadIO m, MonadMask m) => Byline m a -> m (Maybe a)
runByline (Byline byline) = do
  comp <- liftIO (newIORef Nothing)
  let settings = H.setComplete (runCompletionFunction comp) H.defaultSettings

  bracketOnError (liftIO $ H.initializeInput settings) -- Acquire.
                 (liftIO . H.cancelInput)              -- Release.
                 (go comp)                             -- Use.
  where
    go comp state = do
      modes  <- liftIO (H.queryInput state defRenderMode)
      output <- runMaybeT $ runReaderT byline (defEnv state modes comp)

      liftIO (H.closeInput state)
      return output
