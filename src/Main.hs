module Main where

import           BasicPrelude            hiding (head)
import qualified BitMEX                  as Mex
import           BitMEXClient
import           Bot
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
    (pubPath:privPath:_) <- Env.getArgs
    pub <- readFile pubPath
    priv <- B.readFile privPath
    logCxt <- Mex.initLogContext
    let config0 =
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
            }
    config <- return config0 >>= withStdoutLoggingWS
    connect config initBot
