-- Plugin.Source
-- Display source for specified identifiers
module Lambdabot.Plugin.Source (theModule) where

import Lambdabot.Plugin
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (pack,unpack,ByteString)

type Env = M.Map ByteString ByteString

theModule = newModule
    { moduleCmds = return
        [ (command "src")
            { help = say helpStr
            , process = \key -> readMS >>= \env -> case fetch (pack key) env of
                _ | M.null env -> say "No source in the environment yet"
                _ |   null key -> say helpStr
                Nothing        -> say . ("Source not found. " ++) =<< io (random insult)
                Just s         -> say (unpack s)
            }
        ]

    -- all the hard work is done to build the src map.
    -- uses a slighly custom Map format
    , moduleSerialize = Just . readOnly $ M.fromList . map pair . splat . P.lines
    }
        where
            pair (a:b) = (a, P.unlines b)
            splat []   = []
            splat s    = a : splat (tail b) where (a,b) = break P.null s

fetch :: ByteString -> Env -> Maybe ByteString
fetch x m = M.lookup x m `mplus`
            M.lookup (P.concat [P.singleton '(', x, P.singleton ')']) m

helpStr :: String
helpStr = "src <id>. Display the implementation of a standard function"