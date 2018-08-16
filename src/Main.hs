module Main where

import           BasicPrelude
import qualified BitMEX                  as Mex
    ( Leverage (..)
    , runDefaultLogExecWithContext
    )
import           BitMEXClient
    ( BitMEXReader (..)
    , BitMEXWrapperConfig (..)
    , Environment (..)
    )
import           Bot.Logging
    ( esLoggingContext
    , initEsLogContext
    )
import qualified Control.Monad.Reader    as R (runReaderT)
import qualified Data.ByteString         as B (readFile)
import           Data.Text               as T (pack)
import           HMMMBot                 (initBot)
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
    logCxt <- initEsLogContext >>= esLoggingContext user pw
    let config =
            BitMEXWrapperConfig
            { environment = MainNet
            , pathREST = Just "/api/v1"
            , pathWS = Just "/realtime"
            , manager = Just mgr
            , publicKey = pub
            , privateKey = priv
            , logExecContext =
                  Mex.runDefaultLogExecWithContext
            , logContext = logCxt
            }
    R.runReaderT
        (run (initBot
             (Mex.Leverage
                  (read (T.pack leverage) :: Double)))) config
