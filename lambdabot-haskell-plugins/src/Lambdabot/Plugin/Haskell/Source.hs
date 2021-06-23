-- Plugin.Source
-- Display source for specified identifiers
module Lambdabot.Plugin.Haskell.Source (sourcePlugin) where

import Lambdabot.Plugin
import Lambdabot.Util
import Control.Monad
import Data.List (sort)
import qualified Data.ByteString.Char8 as P
import qualified Data.Map as M

type Env = M.Map P.ByteString P.ByteString

sourcePlugin :: Module (M.Map P.ByteString P.ByteString)
sourcePlugin = newModule
    { moduleCmds = return
        [ (command "src")
            { help = say helpStr
            , process = \key -> readMS >>= \env -> case M.lookup (normalize (P.pack key)) env of
                _ | M.null env -> say "No source in the environment yet"
                _ |   null key -> say helpStr
                Nothing        -> say . ("Source not found. " ++) =<< randomFailureMsg
                Just s         -> say (P.unpack s)
            }
        ]

    -- all the hard work is done to build the src map.
    -- uses a slightly custom Map format
    , moduleSerialize = Just . readOnly $ M.fromList . map pair . splat . P.lines
    }
        where
            pair (a:b) = (normalize a, P.unlines b)
            pair _     = error "Source Plugin error: not a pair"
            splat []   = []
            splat s    = a : splat (tail b) where (a,b) = break P.null s

normalize :: P.ByteString -> P.ByteString
normalize = P.unwords . sort . map (P.filter (`notElem` "()")) . P.words

helpStr :: String
helpStr = "src <id> or src <type> <methodid>. Display the implementation of a standard function"
