{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The signal story.
-- Posix signals are external events that invoke signal handlers in
-- Haskell. The signal handlers in turn throw dynamic exceptions.  Our
-- instance of MonadError for LB maps the dynamic exceptions to
-- SignalCaughts, which can then be caught by a normal catchError

-- Here's where we do that.
module Lambdabot.Util.Signals
    ( Signal
    , SignalException(..)
    , ircSignalMessage
    , withIrcSignalCatch
    ) where

import Data.Typeable
import Control.Exception (Exception)

#ifdef mingw32_HOST_OS

type Signal = String
newtype SignalException = SignalException Signal deriving Typeable
instance Exception SignalException

ircSignalMessage :: Signal -> [Char]
ircSignalMessage s = s

withIrcSignalCatch :: MonadBaseControl IO m => m () -> m ()
withIrcSignalCatch m = m

#else

import Control.Concurrent.Lifted (myThreadId, newEmptyMVar, putMVar, MVar, ThreadId)
import Control.Exception.Lifted (bracket, throwTo)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control

import System.IO.Unsafe
import System.Posix.Signals

newtype SignalException = SignalException Signal deriving (Show, Typeable)
instance Exception SignalException

--
-- A bit of sugar for installing a new handler
--
withHandler :: MonadBaseControl IO m => Signal -> Handler -> m () -> m ()
withHandler s h m
  = bracket (liftBase (installHandler s h Nothing))
            (liftBase . flip (installHandler s) Nothing)
            (const m)

-- And more sugar for installing a list of handlers
withHandlerList :: MonadBaseControl IO m => [Signal] -> (Signal -> Handler) -> m () -> m ()
withHandlerList sl h m = foldr (withHandler `ap` h) m sl

--
-- Signals we care about. They're all fatal.
--
-- Be careful adding signals, some signals can't be caught and
-- installHandler just raises an exception if you try
--
ircSignalsToCatch :: [Signal]
ircSignalsToCatch = [
    busError,
    segmentationViolation,
    keyboardSignal,
    softwareTermination,
    keyboardTermination,
    lostConnection,
    internalAbort
    ]

--
-- User friendly names for the signals that we can catch
--
ircSignalMessage :: Signal -> [Char]
ircSignalMessage s
   | s==busError              = "SIGBUS"
   | s==segmentationViolation = "SIGSEGV"
   | s==keyboardSignal        = "SIGINT"
   | s==softwareTermination   = "SIGTERM"
   | s==keyboardTermination   = "SIGQUIT"
   | s==lostConnection        = "SIGHUP"
   | s==internalAbort         = "SIGABRT"
-- this case shouldn't happen if the list of messages is kept up to date
-- with the list of signals caught
   | otherwise                  = "killed by unknown signal"

--
-- The actual signal handler. It is this function we register for each
-- signal flavour. On receiving a signal, the signal handler maps the
-- signal to a a dynamic exception, and throws it out to the main
-- thread. The LB MonadError instance can then do its trickery to catch
-- it in handler/catchError
--
ircSignalHandler :: ThreadId -> Signal -> Handler
ircSignalHandler threadid s
  = CatchOnce $ do
        putMVar catchLock ()
        releaseSignals
        throwTo threadid $ SignalException s

--
-- | Release all signal handlers
--
releaseSignals :: IO ()
releaseSignals =
    flip mapM_ ircSignalsToCatch
               (\sig -> installHandler sig Default Nothing)

--
-- Mututally exclusive signal handlers
--
-- This is clearly a hack, but I have no idea how to accomplish the same
-- thing correctly. The main problem is that signals are often thrown
-- multiple times, and the threads start killing each other if we allow
-- the SignalException to be thrown more than once.
{-# NOINLINE catchLock #-}
catchLock :: MVar ()
catchLock = unsafePerformIO newEmptyMVar

--
-- | Register signal handlers to catch external signals
--
withIrcSignalCatch :: MonadBaseControl IO m => m () -> m ()
withIrcSignalCatch m = do
    _ <- liftBase $ installHandler sigPIPE Ignore Nothing
    _ <- liftBase $ installHandler sigALRM Ignore Nothing
    threadid <- myThreadId
    withHandlerList ircSignalsToCatch (ircSignalHandler threadid) m
#endif
