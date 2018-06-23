module Main where

import           BasicPrelude
import qualified BitMEX                  as Mex
    ( runDefaultLogExecWithContext
    )
import           BitMEXClient
    ( BitMEXWrapperConfig (..)
    , Environment (..)
    , connect
    )
import           Bot                     (initBot)
import           Bot.Logging
    ( esLoggingContext
    , initEsLogContext
    )
import qualified Data.ByteString         as B (readFile)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import qualified System.Environment      as Env (getArgs)

-- tickSize = 0.5
-- strategyThreshold = 0.5
-- minPos = -2
-- maxPos = 2

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:esUserPath:esPasswordPath:_) <- Env.getArgs
    pub <- readFile pubPath
    priv <- B.readFile privPath
    user <- readFile esUserPath
    pw <- readFile esPasswordPath
    logCxt <- initEsLogContext
    let logCxtF = esLoggingContext user pw
    let config =
            BitMEXWrapperConfig
            { environment = TestNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            , logContextFunction = logCxtF
            }
    connect config initBot
