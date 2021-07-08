--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
-- Syntactic sugar for developing plugins.
-- Simplifies import lists, and abstracts over common patterns
--
module Lambdabot.Plugin
    ( Module(..)
    , ModuleT
    , newModule

    , LB
    , MonadLB(..)
    , lim80
    , ios80

    , ChanName
    , mkCN
    , getCN

    , Nick(..)
    , ircPrivmsg

    , module Lambdabot.Config
    , module Lambdabot.Config.Core
    , module Lambdabot.Command
    , module Lambdabot.State
    , module Lambdabot.File
    , module Lambdabot.Util.Serial
    ) where

import Lambdabot.Bot
import Lambdabot.ChanName
import Lambdabot.Config
import Lambdabot.Config.Core
import Lambdabot.Command hiding (runCommand, execCmd)
import Lambdabot.File
import Lambdabot.Module
import Lambdabot.Monad
import Lambdabot.Nick
import Lambdabot.State
import Lambdabot.Util
import Lambdabot.Util.Serial

import Codec.Binary.UTF8.String
import Control.Monad
import Control.Monad.Trans
import Data.Char

lim80 :: Monad m => m String -> Cmd m ()
lim80 action = do
    to <- getTarget
    let lim = case nName to of
                  ('#':_) -> (' ':) . unwords . map (limitStr 80 . strip isSpace) . take 3 -- message to channel: be nice
                  _       -> unlines . map (' ':) -- private message: get everything
        spaceOut = lim . lines
        removeControl = filter (\x -> isSpace x || not (isControl x))
    (say =<<) . lift $ liftM (encodeString . spaceOut . removeControl . decodeString) action

-- | convenience, similar to ios but also cut output to channel to 80 characters
-- usage:  @process _ _ to _ s = ios80 to (plugs s)@
ios80 :: MonadIO m => IO String -> Cmd m ()
ios80 = lim80 . io
