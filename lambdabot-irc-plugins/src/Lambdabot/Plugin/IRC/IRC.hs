-- | The plugin-level IRC interface.

module Lambdabot.Plugin.IRC.IRC (ircPlugin) where

import Lambdabot.IRC
import Lambdabot.Logging
import Lambdabot.Monad
import Lambdabot.Plugin
import Lambdabot.Util
import Lambdabot.Config.IRC

import Control.Concurrent.Lifted
import qualified Control.Concurrent.SSem as SSem
import Control.Exception.Lifted as E (SomeException(..), throwIO, catch)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString.Char8 as P
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Lambdabot.Util.Network (connectTo')
import Network.Socket (PortNumber)
import System.IO
import System.Timeout.Lifted
import Data.IORef

data IRCState =
    IRCState {
        password :: Maybe String
    }

type IRC = ModuleT IRCState LB

ircPlugin :: Module IRCState
ircPlugin = newModule
    { moduleCmds = return
        [ (command "irc-connect")
            { privileged = True
            , help = say "irc-connect tag host portnum nickname userinfo.  connect to an irc server"
            , process = \rest ->
                case splitOn " " rest of
                    tag:hostn:portn:nickn:uix -> do
                        pn <- fromInteger `fmap` readM portn
                        lift (online tag hostn pn nickn (intercalate " " uix))
                    _ -> say "Not enough parameters!"
            }
        , (command "irc-persist-connect")
            { privileged = True
            , help = say "irc-persist-connect tag host portnum nickname userinfo.  connect to an irc server and reconnect on network failures"
            , process = \rest ->
                case splitOn " " rest of
                    tag:hostn:portn:nickn:uix -> do
                        pn <- fromInteger `fmap` readM portn
                        lift (online tag hostn pn nickn (intercalate " " uix))
                        lift $ lift $ modify $ \state' -> state' { ircPersists = M.insert tag True $ ircPersists state' }
                    _ -> say "Not enough parameters!"
            }
        , (command "irc-password")
            { privileged = True
            , help = say "irc-password pwd.  set password for next irc-connect command"
            , process = \rest ->
                case splitOn " " rest of
                    pwd:_ -> do
                        modifyMS (\ms -> ms{ password = Just pwd })
                    _ -> say "Not enough parameters!"
            }
        ]
    , moduleDefState = return $ IRCState{ password = Nothing }
    }

----------------------------------------------------------------------
-- Encoding and decoding of messages

-- | 'encodeMessage' takes a message and converts it to a function.
--   giving this function a string will attach the string to the message
--   and output a string containing IRC protocol commands ready for writing
--   on the outgoing stream socket.
encodeMessage :: IrcMessage -> String -> String
encodeMessage msg
  = encodePrefix (ircMsgPrefix msg) . encodeCommand (ircMsgCommand msg)
          . encodeParams (ircMsgParams msg)
  where
    encodePrefix [] = id
    encodePrefix prefix = showChar ':' . showString' prefix . showChar ' '

    encodeCommand cmd = showString cmd

    encodeParams [] = id
    encodeParams (p:ps) = showChar ' ' . showString' p . encodeParams ps

    -- IrcMessage is supposed to contain strings that are lists of bytes, but
    -- if a plugin messes up the encoding then we may end up with arbitrary
    -- Unicode codepoints. This is dangerous (\x10a would produce a newline!),
    -- so we sanitize the message here.
    showString' = showString . map (\c -> if c > '\xFF' then '?' else c)

-- | 'decodeMessage' Takes an input line from the IRC protocol stream
--   and decodes it into a message.  TODO: this has too many parameters.
decodeMessage :: String -> String -> String -> IrcMessage
decodeMessage svr lbn line =
    let (prefix, rest1) = decodePrefix (,) line
        (cmd, rest2)    = decodeCmd (,) rest1
        params          = decodeParams rest2
    in IrcMessage { ircMsgServer = svr, ircMsgLBName = lbn, ircMsgPrefix = prefix,
                    ircMsgCommand = cmd, ircMsgParams = params }
  where
    decodePrefix k (':':cs) = decodePrefix' k cs
      where decodePrefix' j ""       = j "" ""
            decodePrefix' j (' ':ds) = j "" ds
            decodePrefix' j (c:ds)   = decodePrefix' (j . (c:)) ds

    decodePrefix k cs = k "" cs

    decodeCmd k []       = k "" ""
    decodeCmd k (' ':cs) = k "" cs
    decodeCmd k (c:cs)   = decodeCmd (k . (c:)) cs

    decodeParams :: String -> [String]
    decodeParams xs = decodeParams' [] [] xs
      where
        decodeParams' param params []
          | null param = reverse params
          | otherwise  = reverse (reverse param : params)
        decodeParams' param params (' ' : cs)
          | null param = decodeParams' [] params cs
          | otherwise  = decodeParams' [] (reverse param : params) cs
        decodeParams' param params rest@(c@':' : cs)
          | null param = reverse (rest : params)
          | otherwise  = decodeParams' (c:param) params cs
        decodeParams' param params (c:cs) = decodeParams' (c:param) params cs

ircSignOn :: String -> Nick -> Maybe String -> String -> LB ()
ircSignOn svr nickn pwd ircname = do
    maybe (return ()) (\pwd' -> send $ pass (nTag nickn) pwd') pwd
    send $ user (nTag nickn) (nName nickn) svr ircname
    send $ setNick nickn

------------------------------------------------------------------------
--
-- Lambdabot is mostly synchronous.  We have a main loop, which reads
-- messages and forks threads to execute commands (which write responses).
-- OR
-- We have a main loop which reads offline commands, and synchronously
-- interprets them.

online :: String -> String -> PortNumber -> String -> String -> IRC ()
online tag hostn portnum nickn ui = do
    pwd <- password `fmap` readMS
    modifyMS $ \ms -> ms{ password = Nothing }

    let online' = do
        sock    <- io $ connectTo' hostn portnum
        io $ hSetBuffering sock NoBuffering
        -- Implements flood control: RFC 2813, section 5.8
        sem1    <- io $ SSem.new 0
        sem2    <- io $ SSem.new 4 -- one extra token stays in the MVar
        sendmv  <- io newEmptyMVar
        pongref <- io $ newIORef False
        io . void . fork . forever $ do
            SSem.wait sem1
            threadDelay 2000000
            SSem.signal sem2
        io . void . fork . forever $ do
            SSem.wait sem2
            putMVar sendmv ()
            SSem.signal sem1
        fin <- io $ SSem.new 0
        ready <- io $ SSem.new 0
        E.catch
            (registerServer tag (io . sendMsg sock sendmv fin ready))
            (\err@SomeException{} -> io (hClose sock) >> E.throwIO err)
        lb $ void $ forkFinally
            (E.catch
                (readerLoop tag nickn pongref sock ready)
                (\e@SomeException{} -> errorM (show e)))
            (const $ io $ SSem.signal fin)
        lb $ ircSignOn hostn (Nick tag nickn) pwd ui
        void $ forkFinally
            (E.catch
                (pingPongDelay >> pingPongLoop tag hostn pongref sock)
                (\e@SomeException{} -> errorM (show e)))
            (const $ io $ SSem.signal fin)
        void $ fork $ do
            io $ SSem.wait fin
            unregisterServer tag
            io $ hClose sock
            io $ SSem.signal ready
            delay <- getConfig reconnectDelay
            let retry = do
                continue <- lift $ gets $ \st -> (M.member tag $ ircPersists st) && not (M.member tag $ ircServerMap st)
                if continue
                    then do
                        E.catch online'
                            (\e@SomeException{} -> do
                                errorM (show e)
                                io $ threadDelay delay
                                retry
                            )
                    else do
                        chans <- lift $ gets ircChannels
                        forM_ (M.keys chans) $ \chan ->
                            when (nTag (getCN chan) == tag) $
                            lift $ modify $ \state' -> state' { ircChannels = M.delete chan $ ircChannels state' }

            retry
        watch <- io $ fork $ do
            threadDelay 10000000
            errorM "Welcome timeout!"
            SSem.signal fin
        io $ SSem.wait ready
        killThread watch

    online'

pingPongDelay :: IRC ()
pingPongDelay = io $ threadDelay 120000000

pingPongLoop :: String -> String -> IORef Bool -> Handle -> IRC ()
pingPongLoop tag hostn pongref sock = do
    io $ writeIORef pongref False
    io $ P.hPut sock $ P.pack $ "PING " ++ hostn ++ "\r\n"
    pingPongDelay
    pong <- io $ readIORef pongref
    if pong
        then pingPongLoop tag hostn pongref sock
        else errorM "Ping timeout."

readerLoop :: String -> String -> IORef Bool -> Handle -> SSem.SSem -> LB ()
readerLoop tag nickn pongref sock ready = forever $ do
    line <- io $ hGetLine sock
    let line' = filter (`notElem` "\r\n") line
    if "PING " `isPrefixOf` line'
        then io $ P.hPut sock $ P.pack $ "PONG " ++ drop 5 line' ++ "\r\n"
        else void . fork . void . timeout 15000000 $ do
            let msg = decodeMessage tag nickn line'
            if ircMsgCommand msg == "PONG"
                then io $ writeIORef pongref True
                else do
                    when (ircMsgCommand msg `elem` ["001", "MODE"]) $ io $ SSem.signal ready
                    received msg

sendMsg :: Handle -> MVar () -> SSem.SSem -> SSem.SSem -> IrcMessage -> IO ()
sendMsg sock mv fin _ready msg
    | ircMsgCommand msg `elem` ["PASS", "USER", "NICK"] =
      E.catch (do takeMVar mv
                  P.hPut sock $ P.pack $ encodeMessage msg "\r\n")
              (\err -> do errorM (show (err :: IOError))
                          SSem.signal fin)
sendMsg sock mv fin ready msg =
    E.catch (do takeMVar mv
                SSem.withSem ready $
                  P.hPut sock $ P.pack $ encodeMessage msg "\r\n")
            (\err -> do errorM (show (err :: IOError))
                        SSem.signal fin)
