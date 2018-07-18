module Main where

import           BasicPrelude
import qualified BitMEX                  as Mex
    ( Leverage (..)
    , runDefaultLogExecWithContext
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
import           Data.Text               as T (pack)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import qualified System.Environment      as Env (getArgs)

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    (pubPath:privPath:esUserPath:esPasswordPath:leverage:_) <-
        Env.getArgs
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
    connect
        config
        (initBot
             (Mex.Leverage
                  (read (T.pack leverage) :: Double)))
