--
-- | Let's go lambdabot!
--
module Main where

import Lambdabot.Main
import Lambdabot.Plugin.Haskell
import Modules      (modulesInfo)
import qualified Paths_lambdabot as P

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.Char
import Data.Version
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

flags :: [OptDescr (IO (DSum Config Identity))]
flags = 
    [ Option "h?" ["help"]  (NoArg (usage []))                      "Print this help message"
    , Option "e"  []        (arg "<command>" onStartupCmds   strs)  "Run a lambdabot command instead of a REPL"
    , Option "l"  []        (arg "<level>"   consoleLogLevel level) "Set the logging level"
    , Option "t"  ["trust"] (arg "<package>" trustedPackages strs)  "Trust the specified packages when evaluating code"
    , Option "V"  ["version"] (NoArg version)                       "Print the version of lambdabot"
    , Option "X"  []        (arg "<extension>" languageExts strs)   "Set a GHC language extension for @run"
    , Option "n"  ["nice"]  (NoArg noinsult)                        "Be nice (disable insulting error messages)"
    , Option "u"  []        (arg "<user>" consoleUser return)       "Run -e commands as given user"
    ] where 
        arg :: String -> Config t -> (String -> IO t) -> ArgDescr (IO (DSum Config Identity))
        arg descr key fn = ReqArg (fmap (key ==>) . fn) descr
        
        strs = return . (:[])
        
        level str = case reads (map toUpper str) of
            (lv, []):_ -> return lv
            _          -> usage
                [ "Unknown log level."
                , "Valid levels are: " ++ show [DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY]
                ]
        
        noinsult = return (enableInsults ==> False)

versionString :: String
versionString = ("lambdabot version " ++ showVersion P.version)

version :: IO a
version = do
    putStrLn versionString
    exitWith ExitSuccess

usage :: [String] -> IO a
usage errors = do
    cmd <- getProgName
    
    let isErr = not (null errors)
        out = if isErr then stderr else stdout
    
    mapM_ (hPutStrLn out) errors
    when isErr (hPutStrLn out "")
    
    hPutStrLn out versionString
    hPutStr   out (usageInfo (cmd ++ " [options]") flags)
    
    exitWith (if isErr then ExitFailure 1 else ExitSuccess)

-- do argument handling
main :: IO ()
main = do
    (config, nonOpts, errors) <- getOpt Permute flags <$> getArgs
    when (not (null errors && null nonOpts)) (usage errors)
    config' <- sequence config
    dir <- P.getDataDir
    exitWith <=< lambdabotMain modulesInfo $
        [dataDir ==> dir, lbVersion ==> P.version] ++ config'

-- special online target for ghci use
online :: [String] -> IO ()
online strs = do
    dir <- P.getDataDir
    void $ lambdabotMain modulesInfo
        [dataDir ==> dir, lbVersion ==> P.version, onStartupCmds ==> strs]
